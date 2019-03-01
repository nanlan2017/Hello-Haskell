module MonadWriter where

-- import           Data.Monoid
import qualified Control.Monad.Writer          as I
-- ************************************************************************ --


-- newtype Write a = (a,String)
--- ▇▇▇▇▇▇▇★▇★▇★▇★▇★▇★▇★▇★▇★▇★▇  这个附带的context 内容 是 Monoid 中运算的 二元运算——如 String（List 的一个实例）上的++ ————————> 才能符合 Monad Law ?? ）
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")


applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f =
    let (y, newLog) = f x   -- f x 得到 （y, newlog)  -----> y 替换x ，log 叠加
                          in (y, log ++ newLog)   -- 每调用一次，会把附带的log 内容进行append


v1 = (3, "Smallish gang.") `applyLog` isBigGang
            -- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 关键是：我的函数只要 作用于 Wrapper 里实际的那部分内容，就能得到 盒子 
            -- （然后▇★▇★▇★▇★▇★▇★▇★▇★▇★▇★▇★ ▇  自定义 嵌套的两个盒子的内容如何 合并成一层 盒子！）
v2 = (30, "A freaking platoon.") `applyLog` isBigGang
{-
ghci> v1
(False,"Smallish gang.Compared gang size to 9")

ghci> v2
(True,"A freaking platoon.Compared gang size to 9")
-}


applyLog' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
newtype Price = Sum Int  -- Int值的直接 Wrapper

instance Monoid Price where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x+y)


addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)


-- ```````````` test ``````````````````
v3 :: (Food, Price)  ---------- 这吊类型可以作为一个 Monadic 类型
v3 = applyLog' ("beans", Sum 10) addDrink
-- ([Char], Price) -> ([Char] -> (Food, Price)) -> (Food, Price)

{-
ghci> ("beans", Sum 10) `applyLog` addDrink
("milk",Sum {getSum = 35})
ghci> ("jerky", Sum 25) `applyLog` addDrink
("whiskey",Sum {getSum = 124})
ghci> ("dogmeat", Sum 5) `applyLog` addDrink
("beer",Sum {getSum = 35})
-}
-- \ \ \ \ \\ \\ \ \\ \ \ \\ \\\ \ \\ \ \ 
{-
logNumber :: Int -> I.Writer [String] Int
logNumber x = I.runWriter (x, ["Got number: " ++ show x])

multWithLog :: I.Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)
-}
