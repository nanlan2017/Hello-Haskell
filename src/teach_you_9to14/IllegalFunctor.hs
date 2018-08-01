module IllegalFunctor where

{- 定义一个不符合 Functor-Law 的 functor 实现 -}
data CMaybe a = CNothing | CJust Int a
    deriving Show

instance Functor CMaybe where
    fmap func CNothing = CNothing
    fmap func (CJust count x) = CJust (count+1) (func x)  --每调用一次fmap，就给 Int 域加1

{-
Functor Law :
(1)  fmap id = id     -- map id 的时候不应改变值，但实际进行了改变！ 所以违背了 Functor Law ——>  ▇▇▇▇▇▇就破坏了 函数组合的 “引用透明”
(2)  fmap g. fmap h = fmap (g.h)  -- 显然前者调用了两次，Int 域加2；  而后者只调用了一次。  所以不等、违背了 Functor 规范。

证明： 
(1)
-- CNothing
-- CJust 


-}
