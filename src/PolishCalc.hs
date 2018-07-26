module PolishCalc where

import           Data.Char

{-
10- (4+3)*2
逆波兰式：   10 4 3 + 2 * -
运算策略：从左往右遍历，遇到运算符就取其左边两个数运算，并返回一个值
                    遇到数字，则压栈

-}

solveRPN :: (Floating a, Num a, Read a) => String -> a
{-
solveRPN [] = 0
solveRPN expression = 
        if isOperator $ last expression 
            then calc expression
            else solveRPN
             where 
                operators = ['+','-','*','/']
                isOperator x = x `elem operators
                cacl xs = read (last xs) $ op1 op2
                    where op1 = xs !! ((length xs) - 2)
                          op2 = xs !! ((length xs) - 3)
-}
-- P211: "Haskell 的惰性性质，使得对于list 的操作，我们可以用map,filter，fold 替代其他语言中的 for循环/while 循环 "
solveRPN = head . foldl foldingFunction [] . words -- ▇▇▇▇foldl ： 维护的累加用的数据结构在左边，列表中的从左到右下一项为第二个参数
                                -- ▇▇▇▇ 这个算法的核心就是： 这个foldl 用的累加用的数据结构是一个栈，保存的是未消化的操作数
  where
    -- 对于 List数据的 所有形式上的判断（case-就) 都可以用模式匹配实现
    foldingFunction (x : y : ys) "*"          = (x * y) : ys
    foldingFunction (x : y : ys) "+"          = (x + y) : ys
    foldingFunction (x : y : ys) "-"          = (y - x) : ys
    foldingFunction (x : y : ys) "/"          = (y / x) : ys
    foldingFunction (x : y : ys) "^"          = (y ** x) : ys
    foldingFunction (x     : ys) "ln"         = log x : ys
    foldingFunction xs           numberString = read numberString : xs
