-- Slightly odd result due to interchange.
-- ==
-- random input { [10][10][10]i32 } auto output
-- structure distributed { /DoLoop 1 /DoLoop/SegRed 1 /DoLoop/SegMap 1 }

let main = map (\xss -> #[sequential_outer] map i32.sum xss)
