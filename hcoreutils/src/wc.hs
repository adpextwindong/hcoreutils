module Main where

import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe
import Data.List

data WcOpts = WcOpts { appLines :: Bool
                     , appWords :: Bool
                     , appBytes :: Bool
                     , appChars :: Bool
                     , appMaxLineLength :: Bool
                     , appHelp :: Bool
                     , appVersion :: Bool
                     } deriving Show

argpBytes           = switch ( short 'c' <> long "bytes" <> help "print the byte counts" )
argpChars           = switch ( short 'm' <> long "chars" <> help "print the character counts" )
argpLines           = switch ( short 'l' <> long "lines" <> help "print the newline counts" )
argpMaxLineLength   = switch ( short 'L' <> long "max-line-length" <> help "print the maximum display width" )
argpWords           = switch ( short 'w' <> long "words" <> help "print the word counts" )

argpHelp            = switch ( long "help" <> help "display this help and exit" )
argpVersion         = switch ( long "help" <> help "output version information and exit" )

appOptsParser :: Parser WcOpts
appOptsParser = WcOpts
           <$> argpLines
           <*> argpWords
           <*> argpBytes
           <*> argpChars
           <*> argpMaxLineLength
           <*> argpHelp
           <*> argpVersion

pEmptyOpts :: WcOpts -> Bool
pEmptyOpts (WcOpts False False False False False _ _ ) = True
pEmptyOpts _ = False

mergeDefaultOpts opts = WcOpts True True True True False optsH optsV
    where
        optsH = appHelp opts
        optsV = appVersion opts

argpMTargets :: Parser [FilePath]
argpMTargets = many ( argument str (metavar "FILES...") )

data WcApp = WcApp {
                     appOpts :: WcOpts
                   , appTargets :: [FilePath]
                   } deriving Show

appArgsParser :: Parser WcApp
appArgsParser = WcApp
      <$> appOptsParser
      <*> argpMTargets

mainArgs :: IO WcApp
mainArgs = do
    args <- execParser opts
    let opts = appOpts args
    if pEmptyOpts opts
    then return $ WcApp (mergeDefaultOpts opts) (appTargets args)
    else return args
     where
        opts = info (appArgsParser <**> helper)
          ( fullDesc
          <> progDesc "wc - opt test"
          <> header "hey this is the header?" )
--OPTION PARSING----------------------------------------------------------------

countLines = (-1 +) . length . B.lines
countWords = length . B.words
countBytes = B.length
countMaxLineLength = foldl max 0 . fmap B.length . B.lines


--Figure out chars vs bytes handling
type FileWCCount = (Int,Int,Int,Int,Int)
wcBS :: WcOpts -> ByteString -> FileWCCount
wcBS os s = (cLines, cWords, cBytes, cChars, cMaxLineLength)
    where
        cLines = if appLines os then countLines s else 0
        cWords = if appWords os then countWords s else 0
        cBytes = if appBytes os then countBytes s else 0
        cChars = if appBytes os then countBytes s else 0
        cMaxLineLength = if appMaxLineLength os then countMaxLineLength s else 0

totalCounts :: [FileWCCount] -> FileWCCount
totalCounts = foldl (\(a,b,c,d,maxLINESLeft) (x,y,z,t,maxLINESRight) -> (a+x, b+y, c+z, d+t, max maxLINESLeft maxLINESRight)) (0,0,0,0,0)

printCounts :: WcOpts -> (FilePath, FileWCCount) -> IO ()
printCounts os (fname, (l, w, b, c, mL)) = do
                                sequence_ $ intersperse (putStr "\t") $ catMaybes acts
                                putStr ("\t" ++ fname)
                                putStrLn ""
    where acts = [if appLines os then Just (putStr (show l)) else Nothing,
                  if appWords os then Just (putStr (show w)) else Nothing,
                  if appBytes os then Just (putStr (show b)) else Nothing,
                  if appChars os then Just (putStr (show c)) else Nothing,
                  if appMaxLineLength os then Just (putStr (show mL)) else Nothing ]

main :: IO ()
main = do
    args <- mainArgs
    let opts = appOpts args
    print opts
    let fps = appTargets args :: [FilePath]
    files <- mapM B.readFile fps :: IO [ByteString]
    let results = wcBS opts <$> files
    let total = totalCounts results
    sequence_ $ (printCounts opts) <$> zip fps results
    printCounts opts ("total", total)
