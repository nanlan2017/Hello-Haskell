module Exception where

import           System.Environment
-- import           System.IO
-- import           System.IO.Error

import           Control.Exception.Base


-- catch :: IO a -> (IOError -> IO a) -> IO a
main = toTry `catch` handler

-- try block
toTry :: IO ()
toTry = do
    (filename : _) <- getArgs
    contents       <- readFile filename
    putStrLn $ "the file has" ++ show (length (lines contents)) ++ "lines"

handler :: IOError -> IO ()
handler e = putStrLn "error happend!"
