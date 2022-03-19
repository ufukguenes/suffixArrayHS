module SuffixArray where
import Sort

-- create one suffix starting from index i
suffixFrom [] i = error "Index larger then string"
suffixFrom xs 0 = xs
suffixFrom (x:xs) i = suffixFrom xs (i - 1)

-- create suffix for every index
allSuffix xs = map (suffixFrom xs) [0..c] 
  where 
    c = length xs - 1

-- adds the index to each element in the array  i.e. the new element
-- is of the form (element, index)
makeTupel [] i = []
makeTupel [x] i = [(x, i)]
makeTupel (x:xs) i = (x,i): makeTupel xs (i+1)

-- sorts arrays which have tuple as ther elements.
-- the first element of the tuple should be an array/string
sortTupleArrays xs = mergeSort tupleArrayComparritor xs

-- create array of tuple of form (suffix, at index i)
suffixTuple xs = makeTupel (allSuffix xs) 0

-- crates the suffix sorted where the suffix are indexed
sortedSuffix xs = sortTupleArrays (suffixTuple xs)

--creates suffix array by selecting the index of the sorted suffix
suffixArray xs = pickIndex (sortedSuffix xs)
  where 
    pickIndex [] = []
    pickIndex ((x,c):xs) = c: pickIndex xs

