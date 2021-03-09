module Main where

import System.Environment
import Data.List
import Control.Monad

help :: String
help = "Usage: yes [STRING]...\n\
       \  or:  yes OPTION\n\
       \Repeatedly output a line with all specified STRING(s), or 'y'.\n\
       \\n\
       \Haskell coreutils by George Takumi Crary"

version = "yes (Haskellgolf coreutils) 0.0.1\n\
          \License: BSD-2-Clause\n\
          \Written by George Takumi Crary"

main = sequence_ =<< liftM (loopIfNeeded . buildResp) getArgs

buildResp :: [String] -> (String, Bool)
buildResp [] = ("y", True)
buildResp ("--help":xs) = (help, False)
buildResp ("--version":xs) = (version, False)
buildResp xs = (intercalate " " xs, True)

loopIfNeeded :: (String, Bool) -> [IO ()]
loopIfNeeded (resp, True) = repeat . putStrLn . id $ resp
loopIfNeeded (resp, False) = [putStr (id resp)]
