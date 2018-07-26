module HwtExer1 where
import           Data.Char                     as DC_
import           Data.List                     as DL_
-- https://www.vacationlabs.com/haskell/basic-types-and-functions.html#exercises

-- 判断年份是否为闰年
-- 这3个运算肯定是都要做的... 
-- let语句

isLeapYear :: Int -> Bool
isLeapYear year =
    let can4   = mod year 4 == 0
        can100 = year `mod` 100 == 0
        can400 = year `mod` 400 == 0
    in  can4 && not can100 || can400

--Generate a list of first “N” even numbers
-- 用递归也好写
evenList :: Int -> [Int]
evenList 0 = []
evenList n = [ 2 * x | x <- [1 .. n] ]

--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
--The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000. 
multiple35 :: Int -> Int
multiple35 limit =
    sum [ i | i <- [1 .. limit], i `mod` 3 == 0 || i `mod` 5 == 0 ]

-- 我擦！用fold 运算这个列表：让首项*10, 第2项*9。。。不会写？
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇想想数据是如何一步步 映射、变换过来的
-- ▇▇▇▇▇▇▇ 有局部变量（其实是局部的常量）
isbnCheck :: String -> Bool
isbnCheck isbn =
    (numCount == 10)
        && let s = sum (zipWith (*) (reverse [1 .. 10]) numList)
           in  (s `mod` 11) == 0
  where
    charList = filter (/= '-') isbn
    numCount = length charList
    numList  = map digitToInt charList


isPangram :: String -> Bool
isPangram raw_sentence =
    let leftChars = foldl removeIfExist ['a' .. 'z'] sentence
        removeIfExist keptChars c =
            if c `elem` keptChars then delete c keptChars else keptChars
    in  null leftChars
    where sentence = filter (isAlpha . toLower) raw_sentence --Bug : toLower 不会把第一个字母小写


{-
报文：“WE ARE DISCOVERED FLEE AT ONCE”
编码：
1 | W . . . E . . . C . . . R . . . L . . . T . . . E  --> Reads off as "WECRLTE"
2 | . E . R . D . S . O . E . E . F . E . A . O . C .  --> Reads off as "ERDSOEEFEAOC"
3 | . . A . . . I . . . V . . . D . . . E . . . N . .  --> Reads off as "AIVDEN"
输出：“WECRLTEERDSOEEFEAOCAIVDEN”
-}
encodeMessage :: String -> String
encodeMessage raw_str = let in ""
    where str = filter (/= ' ') $ map toUpper raw_str



