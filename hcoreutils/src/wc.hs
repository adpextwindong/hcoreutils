module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Options.Applicative
import Data.Maybe
import Data.List
import Data.Bifunctor
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Control.Monad.Reader
import Control.Exception
import Data.Either

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

--Strictly based on '\n' newlines not '\r' because of ByteString
countLines :: ByteString -> Int
countLines = length . B.lines

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


wcBS :: WcOpts -> ByteString -> FileWCCount
wcBS os s = (cLines, cWords, cBytes, cChars, cMaxLineLength)
    where
        cLines = if appLines os then countLines s else 0
        cWords = if appWords os then countWords s else 0
        cBytes = if appBytes os then countBytes s else 0
        cChars = if appBytes os then countBytes s else 0
        cMaxLineLength = if appMaxLineLength os then countMaxLineLength s else 0

printCounts :: (FilePath, FileWCCount) -> ReaderT WcOpts IO ()
printCounts (fname, (l, w, b, c, mL)) = do
    os <- ask
    liftIO $ do
        let acts = [if appLines os then mPrint l else Nothing,
                  if appWords os then mPrint w else Nothing,
                  if appBytes os then mPrint b else Nothing,
                  if appChars os then mPrint c else Nothing,
                  if appMaxLineLength os then mPrint mL else Nothing ]

        sequence_ $ intersperse (putStr "\t") $ catMaybes acts
        putStr ("\t" ++ fname)
        putStrLn ""
        where mPrint val = Just (putStr (show val))

main :: IO ()
main = do
    args <- mainArgs
    let opts = appOpts args
    let targets = appTargets args
    runReaderT (main' targets) opts
    exitSuccess

eitherReadFile :: (FilePath -> String) -> FilePath -> IO (Either ByteString String)
eitherReadFile fpHandler fp = handle (\e -> do let _ = show (e :: IOException)
                                               return (Right (fpHandler fp)))
                                     (fmap Left (B.readFile fp))

wcNoReadfmt :: FilePath -> String
wcNoReadfmt fp
    | hasTrailingPathSeparator fp = "wc: " ++ fp ++ ": Is a directory"
    | otherwise = "wc: " ++ fp ++ ": No such file or directory"

printResult :: (Either FileWCCount String, FilePath) -> ReaderT WcOpts IO ()
printResult (Right s,_) = liftIO $ print s
printResult (Left cnts, fp) = printCounts (fp, cnts)

main' :: [FilePath] -> ReaderT WcOpts IO()
main' [] = do
    opts <- ask
    stdinContent <- liftIO B.getContents
    let result = wcBS opts stdinContent
    printCounts ("", result)

main' targets = do
    opts <- ask
    eFiles <- liftIO $ mapM (eitherReadFile wcNoReadfmt) targets
    let eResults = first (wcBS opts) <$> eFiles
    let total = totalCounts $ lefts eResults
    sequence_ $ printResult <$> zip eResults targets
    printCounts ("total", total)
