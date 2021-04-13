module Main where

import Data.Monoid
import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
import Control.Monad.Reader
import Data.List

data BnOpts = BnOpts { appSuffix :: String
                      ,appNULTerminated :: Bool
                     } deriving Show

defaultOpts :: BnOpts
defaultOpts = BnOpts [] False

argpNULTerminated :: Parser Bool
argpNULTerminated = switch ( short 'z' <> long "zero" <> help "end each output line with NUL, not newline" )

argpSuffixes :: Parser String
argpSuffixes = strOption (short 's' <> long "suffix"
                                   <> help "remove a trailing SUFFIX; implies -a"
                                   <> metavar "SUFFIX..."
                                   <> value "")

argpMTargets :: Parser [FilePath]
argpMTargets = many ( argument str (metavar "PATHS..."))

appOptsParser :: Parser BnOpts
appOptsParser = BnOpts
           <$> argpSuffixes
           <*> argpNULTerminated

data BnApp = BnApp {
                     appOpts :: BnOpts
                   , appTargets :: [FilePath]
                   } deriving Show

appArgsParser :: Parser BnApp
appArgsParser = BnApp
      <$> appOptsParser
      <*> argpMTargets

optsParse :: ParserInfo BnApp
optsParse =
    info (helper <*> versionOption <*> appArgsParser)
      ( fullDesc <> header "basename - strip directory and suffix from filenames" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.0.1" (short 'v' <> long "version" <> help "Show version")

main :: IO ()
main = do
    args <- execParser optsParse
    let opts = appOpts args
    let targets = appTargets args
    runReaderT (main' targets) opts
    exitSuccess

main' :: [FilePath] -> ReaderT BnOpts IO()
main' [] = do
    liftIO $ do
        print "basename: missing operand"
        print "Try 'basename --help' for more information."

main' targets = do
    opts <- ask
    let results = basenamed opts <$> targets
    liftIO $ if appNULTerminated opts
             then mapM_ putStr $ intersperse "\NUL" results
             else mapM_ putStrLn results

stripSuffix :: String -> String -> String
stripSuffix suffix = reverse . drop (length suffix) . reverse

basenamed :: BnOpts -> FilePath -> FilePath
basenamed (BnOpts suffix _) fp = stripSuffix suffix fn
    where fn = snd . splitFileName $ fp
