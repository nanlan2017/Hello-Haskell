module Function where

import           Data.List
-- import qualified Data.Map                      as M
import qualified Data.Char
-- import           ListTest                       ( chain )



--  ▇▇▇▇▇ ($) : 中缀函数（函数调用运算符、优先级最低）  ：  可用来去除右边部分的括号！
{-
空格：也是函数调用运算符，但它是左结合的：即
   f a b c 等价于 ((f a) b) c

-}

--   ▇▇▇▇▇▇▇ 函数组合 f.g ：   功能1：
-- 对n 先replicate 5次，再对生成的列表进行sum  (其实已经生成了组合出了一个新函数)
repsum n = sum . replicate 5 $ n

--   ▇▇▇▇▇▇▇ pointless- style： 指定义函数时，无需假设参数名，即 f = 的左边无需写明参数
fn x = ceiling (negate (tan (cos (max 5 x))))

fn' = ceiling . negate . tan . cos . max 5



