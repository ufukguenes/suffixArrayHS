module Sort where

merge comp [] ys = ys
merge comp xs [] = xs
merge comp (x:xs) (y:ys)
  | comp x y = x : merge comp xs (y:ys)
  | otherwise = y : merge comp (x:xs) ys
  
mergeSort comp [] = []
mergeSort comp [x] = [x]
mergeSort comp xs = merge comp (mergeSort comp (firstN half (xs))) (mergeSort comp (fromN half xs))
  where 
    half = div (length xs) 2
    
firstN 0 xs = []
firstN n (x:xs) = x : firstN (n-1) xs

fromN 0 xs = xs
fromN n (x:xs) = fromN (n-1) xs


arrayComparrisor [] ys = True
arrayComparrisor xs [] = False
arrayComparrisor (x:xs) (y:ys) 
    | x < y = True
    | x > y = False
    | otherwise = arrayComparrisor xs ys
    
tupleComparrisor (x,c) (y,i) = arrayComparrisor x y