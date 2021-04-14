module Main where

import Data.Monoid
import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
import Control.Monad.Reader
import Data.List

data BnOpts = BnOptsLong {
                    appLongSuffix :: String
                   ,appLongNULTerminated :: Bool
                   ,appTargets :: [String]
                }
            | BnOptsShort {
                    appTName :: String
                   ,appShortSuffix :: String
                } deriving Show
                     --TODO support the other style: NAME [SUFFIX]

bnOptsShortParser :: Parser BnOpts
bnOptsShortParser = BnOptsShort
                <$> argument str (metavar "NAME")
                <*> argument str (metavar "SUFFIX")

bnOptsLongParser :: Parser BnOpts
bnOptsLongParser = BnOptsLong
                <$> argpSuffix
                <*> argpNULTerminated
                <*> argpMTargets

bnOptsParser :: Parser BnOpts
bnOptsParser = bnOptsShortParser <|> bnOptsLongParser

argpNULTerminated :: Parser Bool
argpNULTerminated = switch ( short 'z' <> long "zero" <> help "end each output line with NUL, not newline" )

argpSuffix :: Parser String
argpSuffix = strOption (short 's' <> long "suffix"
                                   <> help "remove a trailing SUFFIX; implies -a"
                                   <> metavar "SUFFIX..."
                                   <> value "")

argpMTargets :: Parser [FilePath]
argpMTargets = many ( argument str (metavar "PATHS..."))

optsParse :: ParserInfo BnOpts
optsParse =
    info (helper <*> versionOption <*> bnOptsParser)
      ( fullDesc <> header "basename - strip directory and suffix from filenames" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.0.1" (short 'v' <> long "version" <> help "Show version")

main :: IO ()
main = do
    args <- execParser optsParse
    print args
    -- TODO reformulate this to handle the new bnOpts type
    -- runReaderT (main' targets) opts
    exitSuccess

main' :: [FilePath] -> ReaderT BnOpts IO()
main' [] = liftIO $ do
    print "basename: missing operand"
    print "Try 'basename --help' for more information."

main' targets = do
    opts <- ask
    let results = basenamed opts <$> targets
    liftIO $ if appLongNULTerminated opts
             then mapM_ putStr $ intersperse "\NUL" results
             else mapM_ putStrLn results

stripSuffix :: String -> String -> String
stripSuffix suffix = reverse . drop (length suffix) . reverse

basenamed :: BnOpts -> FilePath -> FilePath
basenamed _ fp = undefined
-- basenamed (BnOpts suffix _) fp = stripSuffix suffix fn
--     where fn = snd . splitFileName $ fp
