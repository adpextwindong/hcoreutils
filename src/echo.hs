module Main where

import System.Exit (exitSuccess)
import Options.Applicative
import Data.Bool
import Data.List
import Data.Char
import Numeric

data EchoOpts = EchoOpts {
                    noTrailingNewline :: Bool,
                    interpretBackslashEscapes :: Bool,
                    argEchos :: [String]
                } deriving Show

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

argpInterpret :: Parser Bool
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

main' (EchoOpts pTrail pInterpretBackslashes xs) = do
    let results = if pInterpretBackslashes
                  then interpretAsEscaped <$> xs
                  else putStr <$> xs
    sequence_ $ intersperse (putStr " ") results
    putStr $ bool "\n" "" pTrail

interpretAsEscaped :: String -> IO ()
interpretAsEscaped ('\\':'0':xs) = putChar val >> interpretAsEscaped ( drop (length literal) xs)
    where literal = takeWhile isDigit . take 3 $ xs
          val = chr . fst . head . readHex $ literal

interpretAsEscaped ('\\':'x':xs) = putChar val >> interpretAsEscaped ( drop (length literal) xs)
    where literal = takeWhile isDigit . take 2 $ xs
          val = chr . fst . head . readHex $ literal

interpretAsEscaped ('\\':x:xs) = putStr (escMap x) >> putStr xs

interpretAsEscaped [] = return ()
interpretAsEscaped s = putStr regular >> interpretAsEscaped startsWithEscaped
    where (regular,startsWithEscaped) = break (== '\\') s

escMap :: Char -> String
escMap 'a' = "\a"
escMap 'b' = "\b"
escMap 'e' = "\ESC"
escMap 'f' = "\f"
escMap 'n' = "\n"
escMap 'r' = "\r"
escMap 't' = "\t"
escMap 'v' = "\v"
escMap c = "\\" ++ [c]

-- Seems like \c in any interpretted string kills all further output which means we need to recurse on the args instead of mapM zzz
