module Main where

import System.Environment
import Data.List
import Data.Either
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as SIO


help = T.pack "Usage: yes [STRING]...\n\
              \  or:  yes OPTION\n\
              \Repeatedly output a line with all specified STRING(s), or 'y'.\n\
              \\n\
              \Haskell coreutils by George Takumi Crary"

version = T.pack "yes (Haskellgolf coreutils) 0.0.1\n\
                 \License: BSD-2-Clause\n\
                 \Written by George Takumi Crary"

packMsg :: [String] -> T.Text
packMsg xs = packedMsg
                where
                    msg = intercalate " " xs :: [Char]
                    ln = length msg
                    nFit = ln * (bufferSize `div` ln)
                    packedMsg = T.pack . take nFit . cycle $ msg


buildResp :: [String] -> Either T.Text T.Text
buildResp ("--help":_)      = Right help
buildResp ("--version":_)   = Right version
buildResp []                = Left (packMsg ["y\n"])
buildResp args              = Left (packMsg args)

loopForeverLR :: Either T.Text T.Text -> IO ()
loopForeverLR (Left resp)       = forever . T.putStrLn $ resp
loopForeverLR (Right optionMsg) = T.putStr optionMsg

bufferSize = 4096

main = do
    SIO.hSetBuffering SIO.stdout $ SIO.BlockBuffering (Just bufferSize)
    join $ liftM (loopForeverLR . buildResp) getArgs
