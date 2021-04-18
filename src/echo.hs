module Main where

import System.Exit (exitSuccess)
import Data.Monoid
import Options.Applicative
import Data.Bool
import Data.List

data EchoOpts = EchoOpts {
                    noTrailingNewline :: Bool,
                    interpretBackslashEscapes :: Bool,
                    argEchos :: [String]
                }

defaultOpts :: EchoOpts
defaultOpts = EchoOpts False True []

echoOptParse :: Parser EchoOpts
echoOptParse = EchoOpts
        <$> argpNoTrailingNewline
        <*> (argpInterpret <|> argpDisableBackslashInterpret)
        <*> argpEchos

argpNoTrailingNewline :: Parser Bool
argpNoTrailingNewline = switch ( short 'n' <>
           help "Do not output the trailing newline")

argpInterpret = flag False True ( short 'e' <>
           help "enable interpretation of backslash escapes")

argpDisableBackslashInterpret :: Parser Bool
argpDisableBackslashInterpret = flag' True ( short 'E' <>
           help "disable interpretation of backslash escapes (default)")

argpEchos :: Parser [String]
argpEchos = many ( argument str (metavar "STRINGS..." ))
optsParse :: ParserInfo EchoOpts
optsParse =
    info (helper <*> versionOption <*> echoOptParse)
      ( fullDesc <> header "echo - Echo the STRING(s) to standard output" <>
        footer "If -e is in effect, the following sequences are recognized: [\\\\,\\a,\\b,\\c,\\e,\\f,\\n,\\r,\\t,\\v,\\0NNN,\\xHH] \n NOTE: your shell may have its own version of echo which usually supersedes the version described here.  Please refer to your shell's documentation for details about the options it supports."
        -- TODO figure out how to pretty print this
        <> progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "0.0.1" (short 'v' <> long "version" <>
                                            help "Show version")

main :: IO ()
main = do
    args <- execParser optsParse
    main' args
    --TODO impl
    exitSuccess

main' :: EchoOpts -> IO ()
main' (EchoOpts _ _ []) = return ()
main' (EchoOpts pTrail False xs) = do
    mapM_ putStr $ intersperse " " xs
    putStr $ bool "\n" "" pTrail

main' (EchoOpts pTrail pInterpretBackslashes xs) = undefined --TODO backslash handling
