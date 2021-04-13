module Main where

import System.Exit (exitSuccess)
import Data.Monoid
import Options.Applicative

optsParse :: ParserInfo (a -> a)
optsParse =
    info (helper <*> versionOption)
      ( fullDesc <> header "true - do nothing, successfully" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "1.0.0" (short 'v' <> long "version" <>
                                            help "Show version")

main :: IO ()
main = do
    _ <- execParser optsParse
    exitSuccess
