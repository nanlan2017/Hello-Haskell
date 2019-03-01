module Q15 where

import           Data.List
import           System.Random
import           Control.Monad                  ( replicateM )
-- ******************************************************************************************************
{-
Problem 23
Extract a given number of randomly selected elements from a list.

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-- 核心： 从 小于N 的自然数中 随机挑选m 个数字（m<N)
-}
-- Haskell 中不存在全局的数据，————> 全局的都是utilty 性质的工具函数
-- 如果你的方法要修改一个对象 A：则肯定是把 A 当做参数（旧状态）、并返回 A（新状态）
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select list n
    | n < 0 = error "N must be greater than zero."
    | otherwise = do
        pos <- replicateM n $ getStdRandom $ randomR (0, (length list) - 1)
        return [ list !! p | p <- pos ]
{- 
▇▇▇▇▇▇▇是不是这种改变了全局状态的，就得放在do-块里做（那么类型签名里就出现 IO)
▇▇▇▇▇▇▇好像有一个全局的stdGenerator(随机数种子)一样：
1.  randomR (x,y) ： 为这个随机数种子设定随机数的范围
2.  getStdRandom:  从此随机数种子中获取随机数（并更新这个stdGen)
3.  replicateM n func  : 
4.  gen <- getStdGen  : 获取这个全局的随机数种子
5.  randomRs  (x,y) 和 Generator --->  一个数
-}
rnd_select' :: [a] -> Int -> IO [a]
rnd_select' xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen ]
-- ******************************************************************************************************
{-
Problem 24
Lotto: Draw N different random numbers from the set 1..M.

从指定范围中获取一定数量的随机数

diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
-}
diff_select :: Int -> Int -> IO [Int]
diff_select cnt n = do
    gen <- getStdGen
    return $ take cnt $ randomRs (1, n) gen
-- ******************************************************************************************************
-- 我已经知道怎么得到一个范围内的随机数字了 （利用stdGen)
{-
题25： 获取 List 元素的一种随机重新排列
-}
rnd_permu :: [a] -> IO [a]
rnd_permu []       = return []
rnd_permu (x : xs) = do
    rand <- randomRIO (0, (length xs)) -- 获取一个随机的下标
    rest <- rnd_permu xs -- 获取剩下元素的一个随机排列rest
    return $ let (ys, zs) = splitAt rand rest in ys ++ (x : zs) -- 把第一个元素 按 随机下标 插入到rest
-- ******************************************************************************************************
{-
题26  获取元素的所有组合：比如从5个元素中任取3个元素的所有可能组合结果
> combinations 3 "abcdef"
["abc","abd","abe",...]
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
    [ xs !! i : x
    | i <- [0 .. (length xs) - 1]
    , x <- combinations (n - 1) (drop (i + 1) xs)
    ] --
-- ******************************************************************************************************
-- ▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ List 的sort 和generic 系列api很好用！
{-
题28 list 按元素统计 重排序
-- a. 按长度
lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude>["o","de","de","mn","abc","fgh","ijkl"]
-- b. 按其长度出现的频率：
lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]  --长度4的只出现了一个，故把长度4的排前面
-}

lsort :: [String] -> [String]
lsort = sortBy (\x y -> compare (length x) (length y))

-- b.按长度的频率
-- 先求所有的长度值的集合：   sort (按大小)  . removeDuplicate .  map length $ list
-- 在
