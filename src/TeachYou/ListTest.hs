module ListTest where

------------------ list 的模式匹配  ----------------------
{-
[1,2,3]本质就是 1:2:3:[]的语法糖
那么你就可以用：匹配list 的任意元素
    (x:xs)说明这个list 起码有个x 这1个元素
-}

------------------------ map ,filter ------------------------
-- ★  map ,filter, takeWhile
var1 = map (+ 3) [1 .. 4]
var2 = map ($ 3) [(+ 2), (+ 3)]
-- var3 = map (3) [(+2),(+3)]
var4 = filter (> 3) [1 .. 5]

{-
Collatz序列：
 任取一个自然数n，为偶数even则除以2，为奇数odd 则乘以3加1，得到一个序列，为“n的collatz 序列”
求：100以内，collatz 序列长度大于15的n
-}
-- chain n :: 以n 开头的那个collatz 序列
chain :: Int -> [Int]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
        | odd n  = n : chain (n * 3 + 1)

varls :: Int
varls = length $ filter isLong (map chain [1 .. 100])
    where isLong xs = length xs >= 15
------------------------ fold , scan ------------------------------
-- ▇▇▇▇▇▇▇ 所有遍历list，并据此返回一个数据（包括一个 List）的操作，都可以用fold 实现。
--  ▇▇▇ ★ 其实就是遍历list中元素、对其进行迭代运算的过程
{-
来由：常见的处理：
使用模式匹配处理list 时，通常用[]来作为边界值，
剩下的是用 (x:xs)来分别处理 首元素和 剩下的list
抽象出fold 函数


-}

sum' :: (Num a) => [a] -> a
--化简xs   sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0

-- fold-left
-- 判断一个元素是否存在于list 中
elem' :: (Eq a) => a -> [a] -> Bool
-- 这个lambda 会收到的二元是 （当前累计值， xs的接下来的元素）
elem' y = foldl (\acc x -> x == y || acc) False

-- fold-right
-- 用foldr 实现 map 函数
map' :: (a -> b) -> [a] -> [b]
        -- 之所以要从右边开始，是因为 : list 连接只能是 往头部一项一项插入（当然，也可以 list: [x])
map' f = foldr (\x acc -> f x : acc) []

-- foldl1 : 使用第一个元素为起始值
head' :: [a] -> a
head' = foldl1 (\x _ -> x)

head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)


-- scan: 用来跟踪fold 函数（迭代）执行的过程，其结果会记录过程中的值
var5 = scanl (+) 0 [3, 5, 2, 1]
-- [0,3,8,10,11]


