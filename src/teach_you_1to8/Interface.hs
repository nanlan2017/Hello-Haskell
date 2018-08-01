module Interface where

import           Data.Map                      as Map
-- Eq : ==,  /=
-- Show/Read : 可与 String 相互转换


-- 不存在 "g h" 这种写法，两个函数不能孤零零的靠在一起，除非h 的类型是 g 函数的参数（第一个参数）
-- 函数组合： f:: k->b->c  和 g:: m->n->k 的组合 f.g 的类型签名为： m-> n-> k -> b->c
f = Just . (* 3) -- 利用函数 Composition,生成一个新函数


-- Enum
data Day = Monday | Tuesday |Wednsday | Thursday |Friday | Saturday |Sunday
        deriving (Eq,Ord, Show,Read ,Bounded,Enum)


class Eq' a where
        (==) :: a -> a -> Bool  -- 可知：a 是一个具体类型（kind 为*）
        (/=) :: a -> a -> Bool
        x== y = not (x Interface./= y)
        x/= y = not (x Interface.== y)

data TrafficLight = Red | Green | Yellow

-- 把 TrafficLight 注册成为 Eq' 型的类型
instance Eq' TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False

-- 必须是把一个具体的类型注册为该接口的实现，而不能是template 类型
data Maybe' a = Nothing' | Just'  a
-- instance Eq' (Maybe' a) where
--         Just' x == Just' y = x Prelude.== y

---------------------------- Yesno 这个typeclass ---------------------

-------------------------- Functor 这个typeclass -----------------
-- 注册为 Interface 的肯定是一个类型，所以：▇ f 是一个类型（可能是template 类型）
class Functor' f where  -- ▇▇▇▇▇▇▇▇▇▇这样看 f a 是一个具体类型，f 也可以是一个 a-> x 的函数啊！
                                -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇难怪把这个类型叫做 Functor !!
        fmap:: (a->b) -> f a -> f b
        -- 可知：f a 是一个具体类型，所以f 的kind 为 *->*，即：▇ f是一个接收一个类型参数的模板类型


-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ List 是 Functor
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ Maybe 是 Functor
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ Either a 是 Functor
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ Map k 可作为 Functor

