module ApplicativeFunctor where

import           Control.Applicative           as M_applicative


var1 = (+) <$> (+ 3) <*> (* 100) $ 5
-- 首先 5被丢给 +3 和 *100， 产生 8和 500。  然后 + 被套用到8和500，得到 508

var2 = succ 5
var3 = ($) succ 5


var4 = fmap (\x -> [x]) (Just 4)   -- Just [4]

var5 = M_applicative.liftA2 (:) (Just 3) (Just [4])  -- Just [3,4]

-- ---------------------------------------------------------
{- newtype : 用于将现有的类型 包装成新的类型 （注意和data关键字 的比较）-}

newtype Pair b a = Pair {getPair :: (a,b)}

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x , y)


var7 = (Just 5) >>= (\x -> Just (show x))
--  m a   >>=  (a -> m b)  ::  m b   -- 所以，必然是同一种盒子
-- var8 = (Just 5) >>= (\x -> [x])
-- ---------------------------------------------------------

