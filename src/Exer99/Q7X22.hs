module Q7X22 where

import           Control.Monad
import           Control.Applicative
-- ******************************************************************************************************
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
{-
*> :t [1,2,[1,2],3]
[1,2,[1,2],3] :: (Num [a], Num a) => [[a]]
*> :t [1,2,[1,2,[1,2]]]
[1,2,[1,2,[1,2]]] :: (Num [[a]], Num [a], Num a) => [[[a]]]
-}
--- Haskell 中的 List是homogeneous的， 当其为嵌套时，较难处理
--- NestedList 表示一个有嵌套的 List
{-
一、a 是参数类型，需要构建 NestedList 实例时（使用 Elem/List 这两个值构造子）， 才会对应知道a 是具体啥类型
    出现在类型签名中时， Nested a 才能代表一个具体类型（从而代表“参数”：一个类型的值）
二、
    1.    对于类型 NestedList，其起码要定义一个构造函数（必定含有类型签名）【一个新的类型，就是几个其他类型的值的组合】
    2.    NestedList 有一个参数类型a，意味着右边使用的类型里 有的类型是未明确的、用a 类型指代它。
-}
data NestedList a = Elem a | List [NestedList a]
---------  这下就可以区分'单个元素'和‘list 元素’了 
--------- ▇▇▇▇▇（因为它们可以统一用 NestList类型来匹配，isList 就可以生成 类型签名了！)
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
-- ******************************************************************************************************
{-
8 Problem 8
(**) Eliminate consecutive duplicates of list elements.

> compress "aaaabccaadeeee"
"abcade"
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                [命令式]

                                [函数式]

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}




-- ******************************************************************************************************
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
-- ******************************************************************************************************
{-
Problem 10
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
encodeM = map (\(i, c) -> if i > 1 then Multiple i c else Single 1 c) . encode

v05 = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ******************************************************************************************************
{-
P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                                [命令式]

                                [函数式]

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-}
-- ******************************************************************************************************
-- Problem 13
encodeDirect :: String -> [Chs]
encodeDirect = foldr make []
  where
    make c []                    = [(Single 1 c)]
    make c ((Single i cc) : res) = if c == cc
        then (Multiple (i + 1) cc) : res
        else (Single 1 c) : ((Single i cc) : res)
    make c ((Multiple i cc) : res) = if c == cc
        then (Multiple (i + 1) cc) : res
        else (Single 1 c) : ((Multiple i cc) : res)
-- ******************************************************************************************************
{-
Problem 14
(*) Duplicate the elements of a list.

> dupli [1, 2, 3]
[1,1,2,2,3,3]
>>>>>>>>>>>> 思路  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
典型的1个元素到一个元素的映射，直接map 就好了
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-}
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])
-- ******************************************************************************************************
{-
> repli "abc" 3
"aaabbbccc"
-}
repli :: [a] -> Int -> [a]
repli xs 1 = xs
repli xs n = xs ++ repli xs (n - 1)
-- ******************************************************************************************************
{-
dropEvery "abcdefghik" 3
"abdeghk"
-}
dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n = (init $ take n xs) ++ (dropEvery (drop n xs) n)
-- ab ++ f defghik 3
--       de ++ f ghik 3
--             gh ++ f k 3
--                   []+[]
-- abdegh
{- 挑选元素（和位置有关）的事情用 List comprehension 就行 
           （和值有关  则用filter                   -}
dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n =
    [ e | i <- [0 .. (length xs - 1)], (i + 1) `mod` n /= 0, let e = xs !! i ]
-- ******************************************************************************************************
{-
题17
split "abcdefghik" 3
("abc", "defghik")
-}
-- split = splitAt
-- ******************************************************************************************************
{-
题18
slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}
slice :: [a] -> Int -> Int -> [a]
slice xs m n = take (n - m + 1) . drop (m - 1) $ xs
-- ******************************************************************************************************
{-
题19
 rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
-}
rotate xs n = (\(l, r) -> (r ++ l)) . splitAt n $ xs
-- ******************************************************************************************************
{-
removeAt 2 "abcd"
('b',"acd")
-}
removeAt n xs = let (l, r) = splitAt n xs in (tail l, init l ++ r)
