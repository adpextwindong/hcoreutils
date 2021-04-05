module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data WcApp = WcApp { appBytes :: Bool
                   , appChars :: Bool
                   , appLines :: Bool
                   , appMaxLineLength :: Bool
                   , appWords :: Bool
                   , appHelp :: Bool
                   , appVersion :: Bool
                   , appTargets :: [FilePath]
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

appArgsParser :: Parser WcApp
appArgsParser = WcApp
       <$> argpBytes
       <*> argpChars
       <*> argpLines
       <*> argpMaxLineLength
       <*> argpWords
       <*> argpHelp
       <*> argpVersion
       <*> argpMTargets

main :: IO ()
main = print =<< execParser opts
     where
        opts = info (appArgsParser <**> helper)
          ( fullDesc
          <> progDesc "wc - opt test"
          <> header "hey this is the header?" )
