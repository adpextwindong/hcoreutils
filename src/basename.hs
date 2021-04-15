module Main where

import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
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
                                   <> help "remove a trailing SUFFIX"
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
    main' args
    exitSuccess

main' :: BnOpts -> IO()
main' (BnOptsLong _ _ []) = do
    print "basename: missing operand"
    print "Try 'basename --help' for more information."

main' (BnOptsLong suffix nullt targets) = let results = basenamed suffix <$> targets in
                                          if nullt
                                               then mapM_ putStr $ intersperse "NUL" results
                                               else mapM_ putStrLn results

main' (BnOptsShort name suffix) = putStrLn $ basenamed suffix name

stripSuffix :: String -> String -> String
stripSuffix suffix = reverse . drop (length suffix) . reverse

basenamed :: String -> FilePath -> FilePath
basenamed suffix fp = stripSuffix suffix filename
    where filename = snd . splitFileName $ fp
