module Main where

import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO
import Options.Applicative
import Data.Semigroup ((<>))

data WcApp = WcApp {
                     appOpts :: WcOpts
                   , appTargets :: [FilePath]
                   }

data WcOpts = WcOpts { appBytes :: Bool
                     , appChars :: Bool
                     , appLines :: Bool
                     , appMaxLineLength :: Bool
                     , appWords :: Bool
                     , appHelp :: Bool
                     , appVersion :: Bool
                     }
    deriving Show
-- Optparse parsers
argpBytes = switch ( short 'c' <> long "bytes" <> help "print the byte counts" )
argpChars = switch ( short 'm' <> long "chars" <> help "print the character counts" )
argpLines = switch ( short 'l' <> long "lines" <> help "print the newline counts" )
argpMaxLineLength = switch ( short 'L' <> long "max-line-length" <> help "print the maximum display width" )
argpWords = switch ( short 'w' <> long "words" <> help "print the word counts" )
argpHelp = switch ( long "help" <> help "display this help and exit" )
argpVersion = switch ( long "help" <> help "output version information and exit" )

argpMTargets :: Parser [FilePath]
argpMTargets = many ( argument str (metavar "FILES...") )

appOptsParser :: Parser WcOpts
appOptsParser = WcOpts
           <$> argpBytes
           <*> argpChars
           <*> argpLines
           <*> argpMaxLineLength
           <*> argpWords
           <*> argpHelp
           <*> argpVersion

appArgsParser :: Parser WcApp
appArgsParser = WcApp
      <$> appOptsParser
      <*> argpMTargets

mainArgs :: IO WcApp
mainArgs = execParser opts
     where
        opts = info (appArgsParser <**> helper)
          ( fullDesc
          <> progDesc "wc - opt test"
          <> header "hey this is the header?" )
--OPTION PARSING----------------------------------------------------------------

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
    args <- mainArgs
    print $ appTargets args

main' :: IO ()
main' = do
    fps <- getArgs
    files <- mapM B.readFile fps :: IO [ByteString]
    let results = wcBS <$> files
    let total = totalLCB results
    sequence_ $ print <$> zip results fps
    print (total, "total")
