module IOtest where

-- 一个类型是 IO 的，实际是说 这个行为会涉及一个iostream （而 io a中的a 就是io 交换的内容的类型： 输出 IO (),输入 IO a）
----------- ▇▇▇▇▇▇▇ 只要是要经过 IO 交互的行为，其类型都是 IO 、 而且其所有内容都放在do block 里！！！

import           Data.Char
import           Control.Monad                 as Monad


-- ▇▇▇▇▇▇▇▇▇▇ 当一个名字的返回类型/类型 是 IO 类型时，称该名字为一个“IO action"
-- ▇▇▇▇▇▇▇ 放在do 块中： 我接下来这几个io 操作都使用同一个 ostream 对象
main :: IO ()
main = do
    var1 <- return "haha"  -- ▇▇▇▇ return 讲一个 T 包装成 IO<T>，而 <- 与之相反: 从 IO <T>中再取出 T
    putStrLn "hello world"

io1 = do
    putStrLn "what's your name?"
    name <- getLine  -- ▇▇▇▇▇▇▇▇IO String：  即它的类型是 IO（template)，类型参数是 String 【 IO<String>】
                    -- ▇▇▇▇▇▇▇▇ basic_ostream<char>:  后面的类型参数是此 IO 会返回系统内的类型
                    --  ▇▇▇▇▇▇▇▇ <- 运算符：从 IO 中取出其返回的内容
    let bigName = map toUpper name
    putStrLn $ "Hey" ++ bigName ++ "you rock"


io2 :: IO ()
io2 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseSentence line
            io2

reverseSentence :: String -> String
reverseSentence = unwords . map reverse . words

-- when
io3 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        io3

-- sequence
io4 = do
    a <- getLine
    b <- getLine
    print [a, b]

io4' = do
    rs <- Monad.sequence [getLine, getLine]
    print rs

-- mapM  mapM_
ghci1 = Monad.sequence $ map print [1, 2]
ghci1' = mapM print [1, 2]
ghci1'' = mapM_ print [1, 2]

-- forever



-- forM

