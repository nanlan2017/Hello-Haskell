module RecursionWay where

zip' :: [a] -> [b] -> [(a, b)]
zip' _        []       = []
zip' []       _        = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- 快速排序
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted  = quicksort [a| a<-xs, a<=x]
        biggerSorted   = quicksort [a| a<-xs, a>x]
    in smallerSorted ++ [x] ++ biggerSorted

