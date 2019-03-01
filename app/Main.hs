module Main where

import           System.Environment             ( getArgs )
{- *************************************************************************************************** -}

-- main :: IO (）
interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
  where
    mainWith function = do
        args <- getArgs --取命令行参数
        case args of
            [input, output] -> interactWith function input output --取两个参数：输入/输出文件。然后作用function 于它们
            _               -> putStrLn "error: exactly two arguments needed"

    -- replace "id" with the name of our function below
    myFunction = id  {- ★ 这些被测试的函数的类型“需包含” String -> String -}

{- *************************************************************************************************** -}
--              使用的 API：
-- break  
-- *********************************
splitLines :: String -> [String]
splitLines [] = []  --String 就是[Char] （[]作为值可以匹配 String)
splitLines cs =
    -- 得到pre= 直到特殊字符前的那一部分
    let (pre, suf) = break isLineTerminator cs  --先计算 Let中的局部变量
    in  pre : case suf of --将局部变量代入结果表达式
            ('\r' : '\n' : rest) -> splitLines rest
            ('\r'        : rest) -> splitLines rest
            ('\n'        : rest) -> splitLines rest
            _                    -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'
