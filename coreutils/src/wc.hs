module Main where

import System.Environment
import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO

countLines :: String -> String
countLines xs = "okay"

--TODO check out hFileSize
linesFile :: FilePath -> IO String
linesFile fp = SIO.withFile fp SIO.ReadMode ((liftM countLines) . SIO.hGetContents)

main :: IO [IO String]
main = do
    files <- liftM (drop 1) getArgs :: IO [FilePath]
    return $ liftM linesFile files
