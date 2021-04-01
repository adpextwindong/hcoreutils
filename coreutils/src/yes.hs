module Main where

import System.Environment
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified System.IO as SIO

newtype UsageMsg = MkUsageMsg B.ByteString
fromUsageMsg :: UsageMsg -> B.ByteString
fromUsageMsg (MkUsageMsg bs) = bs

help = MkUsageMsg $ B.pack "Usage: yes [STRING]...\n\
              \  or:  yes OPTION\n\
              \Repeatedly output a line with all specified STRING(s), or 'y'.\n\
              \\n\
              \Haskell coreutils by George Takumi Crary"

version = MkUsageMsg $ B.pack "yes (Haskellgolf coreutils) 0.0.1\n\
                 \License: BSD-2-Clause\n\
                 \Written by George Takumi Crary"

packMsg :: [String] -> B.ByteString
packMsg xs = packedMsg
                where
                    msg = intercalate " " xs :: [Char]
                    ln = length msg
                    nFit = ln * (bufferSize `div` ln)
                    packedMsg = B.pack . take nFit . cycle $ msg


buildResp :: [String] -> Either B.ByteString UsageMsg
buildResp ("--help":_)      = Right help
buildResp ("--version":_)   = Right version
buildResp []                = Left (packMsg ["y\n"])
buildResp args              = Left (packMsg args)

loopForeverLR :: Either B.ByteString UsageMsg -> IO ()
loopForeverLR (Left resp)       = forever . B.putStrLn $ resp
loopForeverLR (Right optionMsg) = B.putStr $ fromUsageMsg optionMsg

bufferSize = 4096

main = do
    SIO.hSetBuffering SIO.stdout $ SIO.BlockBuffering (Just bufferSize)
    join $ liftM (loopForeverLR . buildResp) getArgs
