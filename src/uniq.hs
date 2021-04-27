module Main where

import System.Exit (exitSuccess)
import Options.Applicative
-- import Data.Map

data DuplicateHandling = NoDups | AllDups | OneEachDups
    deriving Show

--Maybe these clash
data SeperationHandling = NoGroupSeperation | Prepend | Separate
    deriving Show
data GroupHandling = SeparateGroups | PrependGroups | AppendGroups | BothGroups
    deriving Show

data UniqOpts = UniqOpts {                                  -- Corresponding GNU Uniq flags
                    outputCount      :: Bool,               -- -c --count
                    outputDuplicates :: DuplicateHandling,  -- -d --all-repeated
                    allRepeated      :: SeperationHandling, -- --all-repeated[=METHOD]
                    skipFields       :: Int,                -- -f --skip-fields=N
                    groupMethod      :: GroupHandling,      -- --group[=METHOD]
                    ignoreCase       :: Bool,               -- -i --ignore-case
                    skipChars        :: Int,                -- -s --skip-chars=N
                    uniqueOnly       :: Bool,               -- -u --unique
                    nullTerminated   :: Bool,               -- -z --zero-terminated
                    checkChars       :: Maybe Int           -- -w --check-chars=N
                }deriving Show

defaultOpts :: UniqOpts
defaultOpts = UniqOpts False NoDups NoGroupSeperation 0 SeparateGroups False 0 False False Nothing

optsParser :: Parser UniqOpts
optsParser = undefined --TODO

data UniqArgs = UniqArgs {
                    opts :: UniqOpts,
                    input :: Maybe FilePath,
                    output :: Maybe FilePath
                } deriving Show

argsParser :: Parser UniqArgs
argsParser = UniqArgs
          <$> optsParser
          <*> (optional $ argument str (metavar "INPUT"))
          <*> (optional $ argument str (metavar "OUTPUT"))


optsParse :: ParserInfo (a -> a)
optsParse =
    info (helper <*> versionOption)
      ( fullDesc <> header "uniq" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "1.0.0" (short 'v' <> long "version" <>
                                            help "Show version")

main :: IO ()
main = do
    _ <- execParser optsParse
    exitSuccess
