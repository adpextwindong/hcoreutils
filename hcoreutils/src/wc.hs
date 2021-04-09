module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Data.Maybe
import Data.List
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.Reader

data WcOpts = WcOpts { appLines :: Bool
                     , appWords :: Bool
                     , appBytes :: Bool
                     , appChars :: Bool
                     , appMaxLineLength :: Bool
                     } deriving Show

defaultOpts :: WcOpts --Lines Words Bytes
defaultOpts = WcOpts True True True False False

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

appOptsParser :: Parser WcOpts
appOptsParser = WcOpts
           <$> argpLines
           <*> argpWords
           <*> argpBytes
           <*> argpChars
           <*> argpMaxLineLength

pEmptyOpts :: WcOpts -> Bool
pEmptyOpts (WcOpts False False False False False) = True
pEmptyOpts _ = False

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

optsParse :: ParserInfo WcApp
optsParse =
    info (helper <*> versionOption <*> appArgsParser)
      ( fullDesc <> header "wc - print newline, word, and byte counts for each file" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.0.1" (short 'v' <> long "version" <> help "Show version")

mainArgs :: IO WcApp
mainArgs = do
    args <- execParser optsParse
    if pEmptyOpts $ appOpts args
    then return $ WcApp defaultOpts $ appTargets args
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

totalCounts :: [FileWCCount] -> FileWCCount
totalCounts = foldl (\(a,b,c,d,maxLINESLeft)
                      (x,y,z,t,maxLINESRight)
                   -> (a+x, b+y, c+z, d+t, max maxLINESLeft maxLINESRight)) (0,0,0,0,0)


------------- TODO Reader these two functions as they need WcOpts
wcBS :: WcOpts -> ByteString -> FileWCCount
wcBS os s = (cLines, cWords, cBytes, cChars, cMaxLineLength)
    where
        cLines = if appLines os then countLines s else 0
        cWords = if appWords os then countWords s else 0
        cBytes = if appBytes os then countBytes s else 0
        cChars = if appBytes os then countBytes s else 0
        cMaxLineLength = if appMaxLineLength os then countMaxLineLength s else 0

printCounts :: WcOpts -> (FilePath, FileWCCount) -> IO ()
printCounts os (fname, (l, w, b, c, mL)) = do
                                sequence_ $ intersperse (putStr "\t") $ catMaybes acts
                                putStr ("\t" ++ fname)
                                putStrLn ""
    where mPrint val = Just (putStr (show val))
          acts = [if appLines os then mPrint l else Nothing,
                  if appWords os then mPrint w else Nothing,
                  if appBytes os then mPrint b else Nothing,
                  if appChars os then mPrint c else Nothing,
                  if appMaxLineLength os then mPrint mL else Nothing ]

main :: IO ()
main = do
    args <- mainArgs
    let opts = appOpts args
    let targets = appTargets args
    print opts --TODO get rid of this once everything is stable
    runReaderT (main' targets) opts

main' :: [FilePath] -> ReaderT WcOpts IO()
main' [] = undefined --TODO readfrom STDIN

main' targets = do
    opts <- ask
    files <- liftIO $ mapM B.readFile targets
    --TODO add error handling for files that can't be openned
    let results = wcBS opts <$> files
    let total = totalCounts results
    liftIO $ sequence_ $ printCounts opts <$> zip targets results
    liftIO $ printCounts opts ("total", total)
    liftIO $ exitSuccess
