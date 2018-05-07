-- This seems to fail in some places.  Generated by tail2futhark (prettified a little bit).
--
-- ==
-- input { [67,67,67,65,65,67,66,65,65,68,65,66,67,65,65,67,68,67,65,67,68,68,66,67,68,68,67,67,67,66,65,68,67,66,67,67,67,65,67,67,67,66,67,67,66,65,67,67] }
-- output { true }

let eqb (x: bool) (y: bool): bool =
  (! ((x || y)) || (x && y))
let reshape_int (l: i32) (x: []i32): []i32 =
  let roundUp = ((l + (length x - 1)) / length x) in
  let extend = flatten (replicate (roundUp) (x)) in
  let (v1, _) = split (l) (extend) in
  v1
entry main (nucleotides: []i32): bool =
  let t_v2 = unflatten 8 6 (reshape_int (8*6) nucleotides) in
  let t_v8 = rearrange (2, 0, 1) (unflatten_3d 8 6 4 (reshape_int (8*6*4) "ABCD")) in
  let t_v9 = unflatten_3d 4 8 6 (reshape_int (4*8*6) (flatten t_v2)) in
  let t_v12 = let x = t_v8 in
              let y = t_v9 in
              map2 (\(x: [][]i32) (y: [][]i32): [][]bool ->
                       map2 (\(x: []i32) (y: []i32): []bool ->
                                map2 (==) (x) (y)) (x) (y)) (x) (y) in
  let t_v15 = map (\(x: [][]bool): []bool ->
                   map (\(x: []bool): bool ->
                        reduce (||) (false) (x)) (x)) (t_v12) in
  let t_v18 = map (\(x: [][]bool): []bool ->
                   map (\(x: []bool): bool ->
                        reduce (||) (false) (x)) (x)) (t_v12) in
  let t_v21 = rearrange (0) (map (\(x: []bool): bool ->
                                  reduce (&&) (true) (x)) (rearrange (1, 0) (t_v18))) in
  let t_v26 = reduce (&&) (true) (let x = t_v21 in
                                  let y = [false, false, false, true, false,
                                           true, false, false] in
                                  map2 eqb (x) (y)) in
  t_v26
