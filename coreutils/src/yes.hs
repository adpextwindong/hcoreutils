module Main where

import System.Environment
import Data.List
import Data.Either
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T


help = T.pack "Usage: yes [STRING]...\n\
              \  or:  yes OPTION\n\
              \Repeatedly output a line with all specified STRING(s), or 'y'.\n\
              \\n\
              \Haskell coreutils by George Takumi Crary"

version = T.pack "yes (Haskellgolf coreutils) 0.0.1\n\
                 \License: BSD-2-Clause\n\
                 \Written by George Takumi Crary"

main = sequence_ =<< liftM (loopIfNeeded . buildResp) getArgs

buildResp :: [String] -> Either T.Text T.Text
buildResp ("--help":_) = Right help
buildResp ("--version":_) = Right version

buildResp [] = Left (T.pack "y")
buildResp xs = Left $ T.pack (intercalate " " xs)

loopIfNeeded :: Either T.Text T.Text -> [IO ()]

-- In Data.List repeat is
-- repeat x = xs where xs = x : xs
-- We should see if we're paying the cost for this cons cell and if there a better way
loopIfNeeded (Left resp) = repeat . T.putStrLn $ resp
loopIfNeeded (Right optionMsg) = [T.putStr optionMsg]
