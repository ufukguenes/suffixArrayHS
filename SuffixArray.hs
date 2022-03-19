
-- create one suffix from index i
suffixFrom [] i = error "Index larger then string"
suffixFrom xs 0 = xs
suffixFrom (x:xs) i = suffixFrom xs (i - 1)

-- create suffix for every index
allSuffix xs = map (suffixFrom xs) [0..c] 
  where 
    c = length xs - 1
   
-- adds the index to each element in the array  i.e. the new element
-- is of the form (element, index)
makeTupel [] c = []
makeTupel [x] c = [(x, c)]
makeTupel (x:xs) c = (x,c): makeTupel xs (c+1)

--sorts arrays which have tuple as ther elements.
-- the first element of the tuple should be an array/string again
sortTupleArrays xs = mergeSort tupleComparrisor xs

-- create array of tuple of form (suffix, at index i)
suffixTuple xs = makeTupel (allSuffix xs) 0

-- crates the suffix sorted where the suffix are indexed
sortedSuffix xs = sortTupleArrays (suffixTuple xs)

--creates suffix array
suffixArray xs = pickIndex (sortedSuffix xs)
  where 
    pickIndex [] = []
    pickIndex ((x,c):xs) = c: pickIndex xs


-- just sorting stuff

-- merge operation with adjustable comparrsion function
merge comp [] ys = ys
merge comp xs [] = xs
merge comp (x:xs) (y:ys)
  | comp x y = x : merge comp xs (y:ys)
  | otherwise = y : merge comp (x:xs) ys
  
-- merge sort
mergeSort comp [] = []
mergeSort comp [x] = [x]
mergeSort comp xs = merge comp (mergeSort comp (firstN half (xs))) (mergeSort comp (fromN half xs))
  where 
    half = div (length xs) 2
    
-- first n elements of an array
firstN 0 xs = []
firstN n (x:xs) = x : firstN (n-1) xs

-- last n elements of an array
fromN 0 xs = xs
fromN n (x:xs) = fromN (n-1) xs

-- compares two arrays
arrayComparrisor [] ys = True
arrayComparrisor xs [] = False
arrayComparrisor (x:xs) (y:ys) 
    | x < y = True
    | x > y = False
    | otherwise = arrayComparrisor xs ys
    
-- compares two tuples which have arrays as their first elements
tupleComparrisor (x,c) (y,i) = arrayComparrisor x y