-- 《Learn You A Haskell》
module WhereLet where

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height | bmi <= skinny = "thin!"
                      | bmi <= normal = "ok"
                      | bmi <= fat    = "whale"
                      | otherwise     = "other"
 where
  bmi    = weight / (height ^ 2)
  skinny = 18.5
  normal = 25.0
  fat    = 30.0

palindrome :: [a] -> [a]
palindrome xs = con xs rs
 where
  -- con
  -- i.e.   con [1,2] [5,6] = [1,2,5,6]
  con []       b = b
  con (x : xs) b = x : con xs b
  rs = rev xs []
  -- rev
  -- i.e.   rev [1,2,3] 4 = [3,2,1,4]
  rev []       rs = rs
  rev (x : xs) rs = rev xs (x : rs) -- 将第一个参数list 的首元素移动到第二个参数的头部

