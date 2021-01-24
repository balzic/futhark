module Futhark.CodeGen.Backends.Javascript 
  (genJavascript)
where

import Futhark.MonadFreshNames
import Futhark.IR.Primitive
import qualified Futhark.CodeGen.ImpCode as ImpCode

type EntryPointName = String
type EntryPointType = String
data JSEntryPoint = JSEntryPoint { name :: String,
                                   parameters :: [EntryPointType],
                                   ret :: [EntryPointType]
                                 }
readTypeEnum :: PrimType -> ImpCode.Signedness -> String
readTypeEnum (IntType Int8) ImpCode.TypeUnsigned = "u8"
readTypeEnum (IntType Int16) ImpCode.TypeUnsigned = "u16"
readTypeEnum (IntType Int32) ImpCode.TypeUnsigned = "u32"
readTypeEnum (IntType Int64) ImpCode.TypeUnsigned = "u64"
readTypeEnum (IntType Int8) ImpCode.TypeDirect = "i8"
readTypeEnum (IntType Int16) ImpCode.TypeDirect = "i16"
readTypeEnum (IntType Int32) ImpCode.TypeDirect = "i32"
readTypeEnum (IntType Int64) ImpCode.TypeDirect = "i64"
readTypeEnum (FloatType Float32) _ = "f32"
readTypeEnum (FloatType Float64) _ = "f64"
readTypeEnum ImpCode.Bool _ = "bool"
readTypeEnum Cert _ = error "readTypeEnum: cert"

entryFunctions :: MonadFreshNames m => ImpCode.Definitions op -> m [(ImpCode.Name, ImpCode.FunctionT op)]
entryFunctions prog = 
  let ImpCode.Definitions _ (ImpCode.Functions funs) = prog
      entry_funs = filter (ImpCode.functionEntry . snd) funs
  in return entry_funs

entryFunctionsToJS :: [(ImpCode.Name, ImpCode.FunctionT op)] -> [JSEntryPoint]
entryFunctionsToJS = map entryTypes
    
genJavascript :: MonadFreshNames m => ImpCode.Definitions op -> m String
genJavascript prog = do
  efs <- entryFunctions prog
  let entryPoints = entryFunctionsToJS efs
  return $ javascriptWrapper entryPoints

--main = do
--  let jse = JSEntryPoint { name = "main", parameters = ["i32"], ret = ["i32"] }
--  putStr (javascriptWrapper [jse])

entryTypes :: (ImpCode.Name, ImpCode.Function op) -> JSEntryPoint-- (String, [String], [String])
entryTypes (fname, func) =
  JSEntryPoint
  {
    name = ImpCode.nameToString fname,
    parameters = map desc $ ImpCode.functionArgs func,
    ret = map desc $ ImpCode.functionResult func
  }
  where
    desc (ImpCode.OpaqueValue d _) = d
    desc (ImpCode.TransparentValue (ImpCode.ScalarValue pt s _)) = readTypeEnum pt s
    desc (ImpCode.TransparentValue (ImpCode.ArrayValue _ _ pt s dims)) =
      concat (replicate (length dims) "[]") ++ readTypeEnum pt s

initFunc :: String
initFunc = 
  unlines
  [" function initData(data) {",
  "    var nDataBytes = data.BYTES_PER_ELEMENT",
  "    var dataPtr = Module._malloc(nDataBytes);",
  "    var dataHeap = new Uint8Array(Module.HEAPU8.buffer, dataPtr, nDataBytes);",
  "    dataHeap.set(new Uint8Array(data.buffer));",
  "    return dataHeap",
  "}"]

initDataHeap :: Int -> String -> String
initDataHeap idx arrType = "    var dataHeap" ++ show idx ++ " = initData(new " ++ arrType ++ "(1));"


resDataHeap :: Int -> String -> String
resDataHeap idx arrType = 
  "    var res" ++ show idx ++ " = new " ++ arrType ++ "(dataHeap" ++ show idx ++ ".buffer," ++
  " dataHeap" ++ show idx ++ ".byteOffset, 1);"
                            
javascriptWrapper :: [JSEntryPoint] -> String
javascriptWrapper entryPoints = unlines 
  [cwraps,
  unlines $ map (cwrapEntryPoint . name) entryPoints,
  initFunc,
  classDef,
  constructor,
  (unlines $ map jsWrapEntryPoint entryPoints),
  endClassDef]

  
cwraps :: String
cwraps = 
  unlines
  ["futhark_context_config_new = Module.cwrap(",
   "  'futhark_context_config_new', 'number', []",
   ");",
   "",
   "futhark_context_new = Module.cwrap(",
   "  'futhark_context_new', 'number', ['number']",
   ");",
   "",
   "futhark_context_sync = Module.cwrap(",
   "  'futhark_context_sync', 'number', ['number']",
   ");"]


classDef :: String
classDef = "class FutharkContext {"

endClassDef :: String
endClassDef = "}"

constructor :: String
constructor = 
  unlines
  ["  constructor() {",
   "    this.cfg = futhark_context_config_new();",
   "    this.ctx = futhark_context_new(this.cfg);",
   "  }"]


jsWrapEntryPoint :: JSEntryPoint -> String
jsWrapEntryPoint jse =
  unlines
  ["  " ++ func_name ++ "(" ++ args ++ ") {",
  -- cwrapEntryPoint func_name,
  inits,
  "    futhark_entry_" ++ func_name ++ "(this.ctx, " ++ rets ++ ", " ++ args ++ ");",
  results,  
  "    futhark_context_sync(this.ctx);",
  "    return [" ++ res ++ "];",
  "}"]
  where
    func_name = name jse
    convTypes = map typeConversion $ ret jse
    inits = unlines $ map (\i -> initDataHeap i (convTypes !! i)) [0..(length (ret jse)) - 1]
    results = unlines $ map (\i -> resDataHeap i (convTypes !! i)) [0..(length (ret jse)) - 1]
    rets = tail (unwords [",dataHeap" ++ show i ++ ".byteOffset" | i <- [0..((length (parameters jse)) - 1)]])
    args = tail (unwords [",in" ++ show i | i <- [1..length (parameters jse)]])
    res = tail (unwords [",res" ++ show i ++ "[0]" | i <- [0..((length (parameters jse)) - 1)]])
 
    
cwrapEntryPoint :: EntryPointName -> String
cwrapEntryPoint ename = 
  unlines 
  ["    futhark_entry_" ++ ename ++ " = Module.cwrap(", 
   "      'futhark_entry_" ++ ename ++ "', 'number', ['number', 'number', 'number']",
   "    );"]


typeConversion :: String -> String
typeConversion typ =
  case typ of 
    "i8" -> "Int8Array"
    "i16" -> "Int16Array"
    "i32" -> "Int32Array"
    "i64" -> "BigInt64Array"
    "u8" -> "Uint8Array"
    "u16" -> "Uint16Array"
    "u32" -> "Uint32Array"
    "u64" -> "BigUint64Array"
    "f32" -> "Float32Array"
    "f64" -> "Float64Array"
    _ -> typ
