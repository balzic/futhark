let main =
	let xs = [[1i32,2i32],[3i32,4]]
	let a = reduce (+) 0 (map (\x -> reduce (+) 0 x) xs)
	in a
