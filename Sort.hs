module Sort where

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
arrayComparritor [] ys = True
arrayComparritor xs [] = False
arrayComparritor (x:xs) (y:ys) 
    | x < y = True
    | x > y = False
    | otherwise = arrayComparritor xs ys
    
-- compares two tuples which have arrays as their first elements
tupleArrayComparritor (x,c) (y,i) = arrayComparritor x y