module Futhark.CodeGen.ImpGen.Multicore.SegHist
  ( compileSegHist,
  )
where

import Control.Monad
import Data.List
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.CodeGen.ImpGen.Multicore.Base
import Futhark.CodeGen.ImpGen.Multicore.SegRed (compileSegRed')
import Futhark.IR.MCMem
import Futhark.MonadFreshNames
import Futhark.Util (chunks, splitFromEnd, takeLast)
import Futhark.Util.IntegralExp (rem)
import Prelude hiding (quot, rem)

compileSegHist ::
  Pattern MCMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.Code
compileSegHist pat space histops kbody nsubtasks
  | [_] <- unSegSpace space =
    nonsegmentedHist pat space histops kbody nsubtasks
  | otherwise =
    segmentedHist pat space histops kbody

-- | Split some list into chunks equal to the number of values
-- returned by each 'SegBinOp'
segHistOpChunks :: [HistOp lore] -> [a] -> [[a]]
segHistOpChunks = chunks . map (length . histNeutral)

onOpCas :: HistOp MCMem -> MulticoreGen ([VName] -> [Imp.TExp Int32] -> MulticoreGen ())
onOpCas op = do
  atomics <- hostAtomics <$> askEnv
  let lambda = histOp op
      do_op = atomicUpdateLocking atomics lambda
  case do_op of
    AtomicPrim f -> return f
    AtomicCAS f -> return f
    AtomicLocking f -> do
      let num_locks = 100151
          dims =
            map toInt32Exp $
              shapeDims (histShape op) ++ [histWidth op]
      locks <-
        sStaticArray "hist_locks" DefaultSpace int32 $
          Imp.ArrayZeros num_locks
      let l' = Locking locks 0 1 0 (pure . (`rem` fromIntegral num_locks) . flattenIndex dims)
      return $ f l'

nonsegmentedHist ::
  Pattern MCMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  TV Int32 ->
  MulticoreGen Imp.Code
nonsegmentedHist pat space histops kbody num_histos = do
  let ns = map snd $ unSegSpace space
      ns_64 = map (sExt64 . toInt32Exp) ns
      num_histos' = tvExp num_histos
      hist_width = toInt32Exp $ histWidth $ head histops
      use_small_dest_histogram = sExt64 num_histos' * sExt64 hist_width .<=. product ns_64

  histops' <- renameHistOpLambda histops

  collect $ do
    flat_idx <- dPrim "iter" int64
    sIf
      use_small_dest_histogram
      (smallDestHistogram pat flat_idx space histops num_histos kbody)
      (casHistogram pat space histops' kbody)

-- |
-- A different version of segHist
-- Takes sequential version and simpley chunks is
-- Implementation does currently not ensure amotic updates
casHistogram ::
  Pattern MCMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen ()
casHistogram pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt64 . toInt32Exp) ns
  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat

  atomicOps <- mapM onOpCas histops

  idx <- dPrim "iter" int64
  body <- collect $ do
    zipWithM_ dPrimV_ is $ map sExt32 $ unflattenIndex ns_64 $ tvExp idx
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
          perOp = chunks $ map (length . histDest) histops
          (buckets, vs) = splitAt (length histops) red_res

      let pes_per_op = chunks (map (length . histDest) histops) all_red_pes
      forM_ (zip5 histops (perOp vs) buckets atomicOps pes_per_op) $
        \(HistOp dest_w _ _ _ shape lam, vs', bucket, do_op, dest_res) -> do
          let (_is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
              dest_w' = toInt32Exp dest_w
              bucket' = toInt32Exp $ kernelResultSubExp bucket
              bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

          sComment "save map-out results" $
            forM_ (zip map_pes map_res) $ \(pe, res) ->
              copyDWIMFix (patElemName pe) (map Imp.vi32 is) (kernelResultSubExp res) []

          sComment "perform updates" $
            sWhen bucket_in_bounds $ do
              let bucket_is = map Imp.vi32 (init is) ++ [bucket']
              dLParams $ lambdaParams lam
              sLoopNest shape $ \is' -> do
                forM_ (zip vs_params vs') $ \(p, res) ->
                  copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
                do_op (map patElemName dest_res) (bucket_is ++ is')

  free_params <- freeParams body (segFlat space : [tvVar idx])
  emit $ Imp.Op $ Imp.ParLoop "atomic_seg_hist" (tvVar idx) mempty body mempty free_params $ segFlat space

segmentedHist ::
  Pattern MCMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
segmentedHist pat space histops kbody = do
  emit $ Imp.DebugPrint "Segmented segHist" Nothing

  -- iteration variable
  n_segments <- dPrim "segment_iter" $ IntType Int64
  collect $ do
    par_body <- compileSegHistBody (tvExp n_segments) pat space histops kbody
    free_params <- freeParams par_body [segFlat space, tvVar n_segments]
    let (body_allocs, body') = extractAllocations par_body
    emit $ Imp.Op $ Imp.ParLoop "segmented_hist" (tvVar n_segments) body_allocs body' mempty free_params $ segFlat space

updateHisto :: HistOp MCMem -> [VName] -> [Imp.TExp Int32] -> MulticoreGen ()
updateHisto op arrs bucket = do
  let acc_params = take (length arrs) $ lambdaParams $ histOp op
      bind_acc_params =
        forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
          copyDWIMFix (paramName acc_p) [] (Var arr) bucket
      op_body = compileBody' [] $ lambdaBody $ histOp op
      writeArray arr val = copyDWIMFix arr bucket val []
      do_hist = zipWithM_ writeArray arrs $ bodyResult $ lambdaBody $ histOp op

  sComment "Start of body" $ do
    dLParams acc_params
    bind_acc_params
    op_body
    do_hist

-- Generates num_threads sub histograms of the size
-- of the destination histogram
-- Then for each chunk of the input each subhistogram
-- is computed and finally merged through a reduction
-- across the histogram indicies.
-- This is expected to be fast if len(histDest) is small
smallDestHistogram ::
  Pattern MCMem ->
  TV Int32 ->
  SegSpace ->
  [HistOp MCMem] ->
  TV Int32 ->
  KernelBody MCMem ->
  MulticoreGen ()
smallDestHistogram pat flat_idx space histops num_histos kbody = do
  emit $ Imp.DebugPrint "smallDestHistogram segHist" Nothing

  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt64 . toInt32Exp) ns

  let pes = patternElements pat
      num_red_res = length histops + sum (map (length . histNeutral) histops)
      map_pes = drop num_red_res pes

  let per_red_pes = segHistOpChunks histops $ patternValueElements pat

  -- Allocate array of subhistograms in the calling thread.  Each
  -- tasks will work in its own private allocations (to avoid false
  -- sharing), but this is where they will ultimately copy their
  -- results.
  global_subhistograms <- forM histops $ \histop ->
    forM (histType histop) $ \t -> do
      let shape = Shape [tvSize num_histos] <> arrayShape t
      sAllocArray "subhistogram" (elemType t) shape DefaultSpace

  let tid' = Imp.vi32 $ segFlat space
      flat_idx' = tvExp flat_idx

  (local_subhistograms, prebody) <- collect' $ do
    zipWithM_ dPrimV_ is $ map sExt32 $ unflattenIndex ns_64 $ sExt64 flat_idx'

    forM (zip per_red_pes histops) $ \(pes', histop) -> do
      op_local_subhistograms <- forM (histType histop) $ \t ->
        sAllocArray "subhistogram" (elemType t) (arrayShape t) DefaultSpace

      forM_ (zip3 pes' op_local_subhistograms (histNeutral histop)) $ \(pe, hist, ne) ->
        -- First thread initializes histogram with dest vals. Others
        -- initialize with neutral element
        sIf
          (tid' .==. 0)
          (copyDWIMFix hist [] (Var $ patElemName pe) [])
          ( sFor "i" (toInt32Exp $ histWidth histop) $ \i ->
              sLoopNest (histShape histop) $ \vec_is ->
                copyDWIMFix hist (i : vec_is) ne []
          )

      return op_local_subhistograms

  -- Generate loop body of parallel function
  body <- collect $ do
    zipWithM_ dPrimV_ is $ map sExt32 $ unflattenIndex ns_64 $ sExt64 flat_idx'
    compileStms mempty (kernelBodyStms kbody) $ do
      let (red_res, map_res) = splitFromEnd (length map_pes) $ kernelBodyResult kbody
          (buckets, vs) = splitAt (length histops) red_res
          perOp = chunks $ map (length . histDest) histops

      sComment "save map-out results" $
        forM_ (zip map_pes map_res) $ \(pe, res) ->
          copyDWIMFix
            (patElemName pe)
            (map Imp.vi32 is)
            (kernelResultSubExp res)
            []

      forM_ (zip4 histops local_subhistograms buckets (perOp vs)) $
        \( histop@(HistOp dest_w _ _ _ shape lam),
           histop_subhistograms,
           bucket,
           vs'
           ) -> do
            let bucket' = toInt32Exp $ kernelResultSubExp bucket
                dest_w' = toInt32Exp dest_w
                bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'
                vs_params = takeLast (length vs') $ lambdaParams lam
                bucket_is = [bucket']

            sComment "perform updates" $
              sWhen bucket_in_bounds $ do
                dLParams $ lambdaParams lam
                sLoopNest shape $ \is' -> do
                  forM_ (zip vs_params vs') $ \(p, res) ->
                    copyDWIMFix (paramName p) [] (kernelResultSubExp res) is'
                  updateHisto histop histop_subhistograms (bucket_is ++ is')

  -- Copy the task-local subhistograms to the global subhistograms,
  -- where they will be combined.
  postbody <- collect $
    forM_ (zip (concat global_subhistograms) (concat local_subhistograms)) $
      \(global, local) -> copyDWIMFix global [tid'] (Var local) []

  free_params <- freeParams (prebody <> body <> postbody) (segFlat space : [tvVar flat_idx])
  let (body_allocs, body') = extractAllocations body
  emit $ Imp.Op $ Imp.ParLoop "seghist_stage_1" (tvVar flat_idx) (body_allocs <> prebody) body' postbody free_params $ segFlat space

  forM_ (zip3 per_red_pes global_subhistograms histops) $ \(red_pes, hists, op) -> do
    bucket_id <- newVName "bucket_id"
    subhistogram_id <- newVName "subhistogram_id"

    let num_buckets = histWidth op

    let segred_space =
          SegSpace (segFlat space) $
            segment_dims
              ++ [(bucket_id, num_buckets)]
              ++ [(subhistogram_id, tvSize num_histos)]

    let segred_op = SegBinOp Noncommutative (histOp op) (histNeutral op) (histShape op)
        ns_red = map (toInt32Exp . snd) $ unSegSpace segred_space
        iterations = case unSegSpace segred_space of
          [_] -> product ns_red
          _ -> product $ init ns_red -- Segmented reduction is over the inner most dimension
    let retvals = map patElemName red_pes
    retvals_ts <- mapM lookupType retvals
    retval_params <- zipWithM toParam retvals retvals_ts
    let retval_names = map Imp.paramName retval_params

    nsubtasks_red <- dPrim "num_tasks" $ IntType Int32
    red_code <- compileSegRed' (Pattern [] red_pes) segred_space [segred_op] nsubtasks_red $ \red_cont ->
      red_cont $
        flip map hists $ \subhisto ->
          ( Var subhisto,
            map Imp.vi32 $
              map fst segment_dims ++ [subhistogram_id, bucket_id]
          )

    let scheduler_info = Imp.SchedulerInfo (tvVar nsubtasks_red) (untyped iterations) Imp.Static
    let red_task = Imp.ParallelTask red_code $ segFlat space
    free_params_red <- freeParams red_code ([segFlat space, tvVar nsubtasks_red] ++ retval_names)
    emit $ Imp.Op $ Imp.Task "seghist_red" free_params_red red_task Nothing retval_params scheduler_info
  where
    segment_dims = init $ unSegSpace space

compileSegHistBody ::
  Imp.TExp Int64 ->
  Pattern MCMem ->
  SegSpace ->
  [HistOp MCMem] ->
  KernelBody MCMem ->
  MulticoreGen Imp.Code
compileSegHistBody idx pat space histops kbody = do
  let (is, ns) = unzip $ unSegSpace space
      ns_64 = map (sExt64 . toInt32Exp) ns

  let num_red_res = length histops + sum (map (length . histNeutral) histops)
      (_all_red_pes, map_pes) = splitAt num_red_res $ patternValueElements pat
      per_red_pes = segHistOpChunks histops $ patternValueElements pat

  collect $ do
    emit $ Imp.DebugPrint "Segmented segHist" Nothing
    let inner_bound = last ns_64

    sFor "i" inner_bound $ \i -> do
      zipWithM_ dPrimV_ (init is) $ map sExt32 $ unflattenIndex (init ns_64) idx
      dPrimV_ (last is) $ sExt32 i

      compileStms mempty (kernelBodyStms kbody) $ do
        let (_red_res, map_res) =
              splitFromEnd (length map_pes) $
                map kernelResultSubExp $ kernelBodyResult kbody
            (buckets, vs) = splitAt (length histops) _red_res
            perOp = chunks $ map (length . histDest) histops

        forM_ (zip4 per_red_pes histops (perOp vs) buckets) $
          \(red_res, HistOp dest_w _ _ _ shape lam, vs', bucket) -> do
            let (is_params, vs_params) = splitAt (length vs') $ lambdaParams lam
                bucket' = toInt32Exp bucket
                dest_w' = toInt32Exp dest_w
                bucket_in_bounds = bucket' .<. dest_w' .&&. 0 .<=. bucket'

            sComment "save map-out results" $
              forM_ (zip map_pes map_res) $ \(pe, res) ->
                copyDWIMFix (patElemName pe) (map Imp.vi32 is) res []

            sComment "perform updates" $
              sWhen bucket_in_bounds $ do
                dLParams $ lambdaParams lam
                sLoopNest shape $ \is' -> do
                  -- Index
                  let buck = toInt32Exp bucket
                  forM_ (zip red_res is_params) $ \(pe, p) ->
                    copyDWIMFix (paramName p) [] (Var $ patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is')
                  -- Value at index
                  forM_ (zip vs_params vs') $ \(p, v) ->
                    copyDWIMFix (paramName p) [] v is'
                  compileStms mempty (bodyStms $ lambdaBody lam) $
                    forM_ (zip red_res $ bodyResult $ lambdaBody lam) $
                      \(pe, se) -> copyDWIMFix (patElemName pe) (map Imp.vi32 (init is) ++ [buck] ++ is') se []
