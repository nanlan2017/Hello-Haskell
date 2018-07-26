module KindSystem where

class Tofu t where
    tofu :: j a -> t a j

{-
    j a 单独出现在->之间，所以kind是 *   
    假设a 是*， 那么j 的kind 是 *-> *
    而 t a j 的kind 是 *
    所以 t 的kind 是 * -> (*->*) -> *   
    【而把t 注册为 Tofu 型，则t 是类型
    ————————>  ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ t是一个接受 一个类型参数+ 一个模板类型参数 的模板 ？？？】
-}

-- 定义一个上述的t 类型: Frank
data Frank a b = Frank {frankField:: b a} deriving (Show)



type T1 = Frank String Maybe
var1 :: T1
var1 = Frank {frankField = Just "HAHA"}


------------------------------------------------------------------
data Barry t  k p = Barry {yabba:: p , dabba:: t k}

-- 推导同上，可知 Barry 的kind 为 (*->*) -> *-> * ->*
-- 现在，把这个类型注册为Functor
-- Funtor 的kind 必须是 *-> *， 所以要 Barry a b 部分实例化后才可以（注意a 是一个模板）
instance Functor (Barry a b) where
    fmap f Barry{yabba = x, dabba = y} = Barry {yabba = f x, dabba= y}
