module Q10 where

{-
1 Problem 1
(*) Find the last element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}
myLast :: [a] -> a
myLast [] = error "last on empty list!"
-- myLast (_ : x : []) = x  -- 模式匹配如何匹配末尾元素？
myLast xs = last xs
-------------------------------------------------------------------------------------------
{-
2 Problem 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}
myButLast :: [a] -> a
myButLast []       = error "empty list!"
myButLast [x]      = error "only one element!"
myButLast [x, _]   = x
myButLast (x : xs) = myButLast xs

-------------------------------------------------------------------------------------------
{-
3 Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example in Haskell:
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
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

-------------------------------------------------------------------------------------------
{-
4 Problem 4
(*) Find the number of elements of a list.

Example in Haskell:

Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}
myLength :: [a] -> Int
myLength xs = case xs of
    []      -> 0
    (x : l) -> 1 + myLength l

myLength' :: [a] -> Int
myLength' xs = length [ 1 | i <- xs ]
-------------------------------------------------------------------------------------------
{-
Reverse a list
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
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



