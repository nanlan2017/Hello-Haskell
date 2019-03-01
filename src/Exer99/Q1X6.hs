module Q1X6 where
import           Control.Monad
import           Control.Applicative
-- ******************************************************************************************************
{-
1 Problem 1
(*) Find the last element of a list.
-}
myLast :: [a] -> a
myLast [] = error "last on empty list!"
-- myLast (_ : x : []) = x  -- 模式匹配如何匹配末尾元素？
myLast xs = last xs
-- ******************************************************************************************************
{-
2 Problem 2
(*) Find the last but one element of a list.
-}
myButLast :: [a] -> a
myButLast []       = error "empty list!"
myButLast [x]      = error "only one element!"
myButLast [x, _]   = x
myButLast (x : xs) = myButLast xs

-- ******************************************************************************************************
{-
3 Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.
-}
elementAt :: [a] -> Int -> a
{-
elementAt xs idx | isOver = error "wrong"
                 | otherwise xs !! idx
        where 
                len = length xs
                isOver = len< xs
-}
elementAt (x : _ ) 1 = x
elementAt (x : xs) i = elementAt xs (i - 1)

-- ******************************************************************************************************
{-
4 Problem 4
(*) Find the number of elements of a list.
-}
myLength :: [a] -> Int
myLength xs = case xs of
    []      -> 0
    (x : l) -> 1 + myLength l

myLength' :: [a] -> Int
myLength' xs = length [ 1 | i <- xs ]
-- ******************************************************************************************************
{-
5 Problem 5
(*) Reverse a list.
-}
myReverse :: [a] -> [a]
myReverse xs = case xs of
    []      -> []
    [x    ] -> [x]
    (x : s) -> myReverse s ++ myReverse [x]

myReverse' :: [a] -> [a]
myReverse' xs = case xs of
    []           -> []
    -- 如何模式匹配到末尾的元素 (_:x:[]) -> x: myReverse' _
    all@(x : li) -> last all : (myReverse' $ init all)

-- ******************************************************************************************************
{-
6 Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-}
isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = Control.Monad.liftM2 (==) id reverse
{-
liftM2 (==) id reverse $ [1,2,3]
-- 把[1,2,3]分别喂给 id ,reverse ，得到 [1,2,3]和 [3,2,1]
-- 再把这两个喂给== 
————————————————————————————————————————
== f g $ x
等价于  f x == g x    
-}

isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) Control.Applicative.<*> reverse
{-
== <*> reverse $ [...]
   <*> [...]  == reverse [...]
-}

