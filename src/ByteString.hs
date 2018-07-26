module ByteString where

import           System.Environment
-- import           System.Directory
import qualified Data.ByteString.Lazy          as I_B


data Fuck = A | But

main = do
    (fileName1 : fileName2 : _) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- I_B.readFile source
    I_B.writeFile dest contents
