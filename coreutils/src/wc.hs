module Main where

import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO

countLines = (-1 +) . length . B.lines
countWords = length . B.words
countBytes = B.length

wcBS :: ByteString -> (Int, Int, Int)
wcBS s = (cLines, cWords, cBytes)
    where 
        cLines = countLines s
        cWords = countWords s
        cBytes = countBytes s

totalLCB :: [(Int,Int,Int)] -> (Int,Int,Int)
totalLCB = foldl (\(a,b,c) (x,y,z) -> (a+x, b+y, c+z)) (0,0,0)

main :: IO ()
main = do
    fps <- getArgs
    files <- mapM B.readFile fps :: IO [ByteString]
    let results = fmap wcBS files :: [(Int, Int, Int)]
    let total = totalLCB results
    sequence_ $ fmap putStrLn $ fmap show $ zip results fps
    putStrLn $ show (total, "total")