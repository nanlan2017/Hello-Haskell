module Q10 where

import           Control.Monad
import           Control.Applicative
-- *************************************************
{-
1 Problem 1
(*) Find the last element of a list.
-}
myLast :: [a] -> a
myLast [] = error "last on empty list!"
-- myLast (_ : x : []) = x  -- 模式匹配如何匹配末尾元素？
myLast xs = last xs
-------------------------------------------------------------------------------------------
{-
2 Problem 2
(*) Find the last but one element of a list.
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
-}
myLength :: [a] -> Int
myLength xs = case xs of
    []      -> 0
    (x : l) -> 1 + myLength l

myLength' :: [a] -> Int
myLength' xs = length [ 1 | i <- xs ]
-------------------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------------------
{-
7 Problem 7
flatten a list.

>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  命令式： flatten :遍历list 中元素，将其中元素append
                   若某个元素 x 为嵌套的 List 元素时，就 append (flatten x)
  函数式： foldr  append []  xs
            where 
                append resAcc  '单个元素' =  resAcc :'单个元素'
                append resAcc  'List'    =  resAcc : flatten 'List'

            -- 如何区分'单个元素'vs 'List'
              isList :: ★▇▇▇▇▇▇▇▇▇▇ 类型签名里就没法表现嵌套 N 层的 List
              isList   [] = True
              isList   (_:_) = True
              otherwise False
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

---------------------- Haskell 中的 List是homogeneous的， 当其为嵌套时，较难处理
{-
*Q10> :t [1,2,[1,2],3]
[1,2,[1,2],3] :: (Num [a], Num a) => [[a]]
*Q10> :t [1,2,[1,2,[1,2]]]
[1,2,[1,2,[1,2]]] :: (Num [[a]], Num [a], Num a) => [[[a]]]
-}

---------------------- NestedList 表示一个有嵌套的 List
{-
一、a 是参数类型，需要构建 NestedList 实例时（使用 Elem/List 这两个值构造子）， 才会对应知道a 是具体啥类型
    出现在类型签名中时， Nested a 才能代表一个具体类型（从而代表“参数”：一个类型的值）
二、
    1.    对于类型 NestedList，其起码要定义一个构造函数（必定含有类型签名）【一个新的类型，就是几个其他类型的值的组合】
    2.    NestedList 有一个参数类型a，意味着右边使用的类型里 有的类型是未明确的、用a 类型指代它。
-}
data NestedList a = Elem a | List [NestedList a]
---------  这下就可以区分'单个元素'和‘list 元素’了 ----------------▇▇▇▇▇（因为它们可以统一用 NestList类型来匹配，isList 就可以生成 类型签名了！)
{-
BNF文法（符号替换）
    nl = var 
         |  [nl,nl, , ,]
则用nl 可化成 全int形式          
-}
v00 = Elem 5
-- [5]
v01 = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
-- 去掉构造函数名Elem/List：   [1, [2,[3,4],5]]
v02 = List []


flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) = foldl (\acc item -> acc ++ flatten item) [] xs  -- concatMap flatten xs
-------------------------------------------------------------------------------------------
{-
9 Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
[命令式]
    数据结构：维护两个list，一个是结果 res-list,一个是当前计算的 temp-list
    "stack" :: ([],[])
    遍历， merge (temp-list,res-list) char :  
         若char 属于temp-list，则把 char 添到temp-list
         否则，就合并 temp-list 到res-list 、并把char 添到空出来的temp-list
[函数式]
    

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-}
-- 测试数据
v03 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]


{-
pack :: [Char] -> [String]
pack charlist = foldl merge (([], [])) charlist
                    where 
        -- merge
                    merge ((tempList, resList)) ch = 
                         | ( null tempList || ch `elem` tempList ) = ((ch:templist, resList))
                         | otherwise = (([ch], resList++tempList))
-}
data Stack = Stack{tempList::[Char], resList::[String]} deriving Show

pack' :: [Char] -> Stack
pack' = foldl merge (Stack [] [])
  where
    merge (Stack templ resl) ch | null templ      = Stack [ch] resl
                                | ch `elem` templ = Stack (ch : templ) resl
                                | otherwise       = Stack [ch] (templ : resl)

pack :: [Char] -> [String]
pack charlist = resList $ foldl merge (Stack [] []) charlist
  where
    -- 有 Bug，最后一个char 完成后未能合并temp-list 到res-list
    merge (Stack templ resl) ch | null templ      = Stack [ch] resl
                                | ch `elem` templ = Stack (ch : templ) resl
                                | otherwise       = Stack [ch] (resl ++ [templ]) --如何把一个元素添加到 List的末尾

{-
▇▇▇▇▇ 操作 List时要时刻想到 递归！
-- 如果有一个 元素 和 已经 operated 好的list 
    能不能 合成结果 ？？  
如果可以，就完全可以化成  operate (x:xs) =  x ### (operate xs)

-}

pack_1 :: (Eq a) => [a] -> [[a]]
pack_1 = foldr func []   -- fold 右边  可以很好使用 ：连接
  where
    func x []       = [[x]]
    func x (y : xs) = if x == (head y) then ((x : y) : xs) else ([x] : y : xs)
-------------------------------------------------------------------------------------------
{-
encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                            [命令式]
pack 好，再map 一下就行。
f :  "aaa" => (3,'a')    \xs -> (length xs, head a)
-}
encode :: String -> [(Int, Char)]
encode = map (\x -> (length x, head x)) . pack_1

-- ******************************************************************************************************

{-
P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
    Multiple 2 'a',Single 'd',Multiple 4 'e']
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                [命令式]
map f :  (4,'a') --->  Multiple 4 'a'
                                [函数式]

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-}

data Chs = Multiple Int Char | Single Int Char deriving Show
encodeM =
    map (\(i, c) -> if i > 1 then Multiple i c else Single 1 c) . Q10.encode

v05 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ******************************************************************************************************

encodeD :: String -> [Chs]
encodeD = foldr make []
  where
    make c []                    = [(Single 1 c)]
    make c ((Single i cc) : res) = if c == cc
        then (Multiple (i + 1) cc) : res
        else (Single 1 c) : ((Single i cc) : res)
    make c ((Multiple i cc) : res) = if c == cc
        then (Multiple (i + 1) cc) : res
        else (Single 1 c) : ((Single i cc) : res)
-- ******************************************************************************************************
