module Main where

import Options.Applicative
import System.Exit (exitSuccess)
import Control.Monad.Reader

data BnOpts = BnOpts { appSuffixes :: [String],
                       appNULTerminated :: Bool
                     } deriving Show

defaultOpts :: BnOpts
defaultOpts = BnOpts [] False

argpNULTerminated :: Parser Bool
argpNULTerminated = switch ( short 'm' <> long "chars" <> help "print the character counts" )

argpSuffixes :: Parser [String]
argpSuffixes = many ( strOption (short 's' <> long "suffix"
                                   <> help "remove a trailing SUFFIX; implies -a"
                                   <> metavar "SUFFIXES..."))

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
    print opts
    print targets
    runReaderT (main' targets) opts
    exitSuccess

main' :: [FilePath] -> ReaderT BnOpts IO()
main' [] = do
    opts <- ask
    return ()
main' targets = do
    opts <- ask
    return ()