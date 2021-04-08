module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Data.Maybe
import Data.List
import System.Exit (exitFailure, exitSuccess)

data WcOpts = WcOpts {
                       appHelp :: Bool
                     , appVersion :: Bool
                     ----------------------
                     , appLines :: Bool
                     , appWords :: Bool
                     , appBytes :: Bool
                     , appChars :: Bool
                     , appMaxLineLength :: Bool
                     } deriving Show

argpBytes :: Parser Bool
argpBytes           = switch ( short 'c' <> long "bytes" <> help "print the byte counts" )
argpChars :: Parser Bool
argpChars           = switch ( short 'm' <> long "chars" <> help "print the character counts" )
argpLines :: Parser Bool
argpLines           = switch ( short 'l' <> long "lines" <> help "print the newline counts" )
argpMaxLineLength :: Parser Bool
argpMaxLineLength   = switch ( short 'L' <> long "max-line-length" <> help "print the maximum display width" )
argpWords :: Parser Bool
argpWords           = switch ( short 'w' <> long "words" <> help "print the word counts" )

argpHelp :: Parser Bool
argpHelp            = switch ( long "help" <> help "display this help and exit" )
argpVersion :: Parser Bool
argpVersion         = switch ( long "help" <> help "output version information and exit" )

appOptsParser :: Parser WcOpts
appOptsParser = WcOpts
           <$> argpHelp
           <*> argpVersion
           <*> argpLines
           <*> argpWords
           <*> argpBytes
           <*> argpChars
           <*> argpMaxLineLength

pEmptyOpts :: WcOpts -> Bool
pEmptyOpts (WcOpts _ _ False False False False False) = True
pEmptyOpts _ = False

mergeDefaultOpts :: WcOpts -> WcOpts          --Lines Words Bytes
mergeDefaultOpts opts = WcOpts optsH optsV True True True True False
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

optsParse = info (appArgsParser <**> helper)
  ( fullDesc
  <> header "wc - print newline, word, and byte counts for each file"
  <> progDesc "Haskell coreutils by George Takumi Crary" --TODO figure out how to build a usage string from this
  )

mainArgs :: IO WcApp
mainArgs = do
    args <- execParser optsParse
    let opts = appOpts args
    if pEmptyOpts opts
    then return $ WcApp (mergeDefaultOpts opts) (appTargets args)
    else return args

--OPTION PARSING----------------------------------------------------------------

countLines :: ByteString -> Int
countLines = (-1 +) . length . B.lines

countWords :: ByteString -> Int
countWords = length . B.words

countBytes :: ByteString -> Int
countBytes = B.length

countMaxLineLength :: ByteString -> Int
countMaxLineLength = foldl max 0 . fmap B.length . B.lines
--TODO Figure out chars vs bytes handling

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
totalCounts = foldl (\(a,b,c,d,maxLINESLeft)
                      (x,y,z,t,maxLINESRight)
                   -> (a+x, b+y, c+z, d+t, max maxLINESLeft maxLINESRight)) (0,0,0,0,0)

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
    print opts --TODO get rid of this once everything is stable
    main' args

main' :: WcApp -> IO()
main' (WcApp (WcOpts True _ _ _ _ _ _) _) = undefined --show help or some shit TODO reorder this
main' (WcApp (WcOpts _ True _ _ _ _ _) _) = undefined --show help or some shit TODO reorder this

main' (WcApp opts []) = undefined --TODO readfrom STDIN

main' (WcApp opts targets) = do
    files <- mapM B.readFile targets :: IO [ByteString]
    --TODO add error handling for files that can't be openned
    let results = wcBS opts <$> files
    let total = totalCounts results
    sequence_ $ printCounts opts <$> zip targets results
    printCounts opts ("total", total)
    exitSuccess
