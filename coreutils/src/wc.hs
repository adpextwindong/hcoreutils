module Main where

import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO

countLines :: String -> String
countLines s = s

--TODO check out hFileSize
linesFile :: FilePath -> IO String
linesFile fp = SIO.withFile fp SIO.ReadMode (fmap countLines . SIO.hGetContents')

main :: IO ()
main = do
    files <- getArgs :: IO [FilePath]
    acts <- mapM linesFile files
    mapM_ putStrLn acts