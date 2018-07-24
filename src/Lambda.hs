module Lambda where

{-
lambda 就是匿名函数，只在当前行使用
\ 后面跟用空格分隔的函数，->后面是函数体

1. 部分时候
(\x -> x+3) 与 (+3) 是一样的，没有后者清晰

2. 
lambda 的参数部分可以使用模式匹配。（只能提供一种模式，若匹配不到则报error)

-}
var1 :: [Int]
var1 = map (\(a, b) -> a + b) [(1, 2), (3, 4)]




addThree :: (Num a) => a -> a -> a -> a
-- addThree x y z = x + y + z
addThree = \x -> \y -> \z -> x + y + z

