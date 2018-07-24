module Condition where

-- ▇▇▇▇▇▇ if-then-else 是个表达式：根据if 条件是否满足，选取一个值
doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100 then x else x * 2

-- ▇▇▇▇▇▇ guards
-- guard 语法: 就是if-else 语句的语法皮 
    -- 生成list，含有指定的n个偶数
getList :: Int -> [Int]
getList n | n == 0    = []
          | otherwise = [ x * 2 | x <- [1 .. n] ]


-- ▇▇▇▇▇▇ 模式匹配 （等同于 C++ 模板的特化匹配）
head :: [a] -> a
head []      = error "No head for empty list"
head (x : _) = x

-- ▇▇▇▇▇▇ case : 模式匹配不过是case 语句的语法糖~
head' :: [a] -> a
head' xs = case xs of
    []      -> error "No head for empty list"
    (x : _) -> x
