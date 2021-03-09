module Main where

import System.Environment
import Data.List
import Data.Either
import Control.Monad

help = "Usage: yes [STRING]...\n\
       \  or:  yes OPTION\n\
       \Repeatedly output a line with all specified STRING(s), or 'y'.\n\
       \\n\
       \Haskell coreutils by George Takumi Crary"

version = "yes (Haskellgolf coreutils) 0.0.1\n\
          \License: BSD-2-Clause\n\
          \Written by George Takumi Crary"

main = sequence_ =<< liftM (loopIfNeeded . buildResp) getArgs

buildResp :: [String] -> Either String String
buildResp ("--help":_) = Right help
buildResp ("--version":_) = Right version

buildResp [] = Left "y"
buildResp xs = Left $ intercalate " " xs

loopIfNeeded :: Either String String -> [IO ()]
loopIfNeeded (Left resp) = repeat . putStrLn . id $ resp
loopIfNeeded (Right optionMsg) = [putStr (id optionMsg)]
