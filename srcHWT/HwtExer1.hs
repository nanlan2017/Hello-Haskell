module HwtExer1 where
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









