module Main where

import System.IO
import System.Exit (exitSuccess)
import Options.Applicative
import Control.Monad
-- import Data.Map
                                                --NoSeperation for allDups
data DuplicateHandling = NoDups | OneEachDups | AllDups | AllRepeated SeperationHandling
    deriving Show

--Maybe these clash
data SeperationHandling = Prepend | Separate
    deriving Show
data GroupHandling = NoGroups | SeparateGroups | PrependGroups | AppendGroups | BothGroups
    deriving Show

data UniqOpts = UniqOpts {                                  -- Corresponding GNU Uniq flags
                    outputCount      :: Bool,               -- -c --count
                    outputDuplicates :: DuplicateHandling,  -- -d --all-repeated
                    skipFields       :: Int,                -- -f --skip-fields=N
                    groupMethod      :: GroupHandling,      -- --group[=METHOD]
                    ignoreCase       :: Bool,               -- -i --ignore-case
                    skipChars        :: Int,                -- -s --skip-chars=N
                    uniqueOnly       :: Bool,               -- -u --unique
                    nullTerminated   :: Bool,               -- -z --zero-terminated
                    checkChars       :: Maybe Int           -- -w --check-chars=N
                }deriving Show

defaultOpts :: UniqOpts
defaultOpts = UniqOpts False NoDups 0 SeparateGroups False 0 False False Nothing

optsParser :: Parser UniqOpts
optsParser = UniqOpts
            <$> outputCountParser
            <*> outputDuplicatesParser
            <*> skipFieldsParser
            <*> groupHandlingParser
            <*> ignoreCaseParser
            <*> skipCharsParser
            <*> uniqueOnlyParser
            <*> nullTerminatedParser
            <*> checkCharsParser

outputCountParser :: Parser Bool
outputCountParser = switch ( short 'c' <> long "count" <> help "TODO prefix lines by the number of occurences" )

-- -d -D --all-Repeated[GROUPS]
outputDuplicatesParser :: Parser DuplicateHandling
outputDuplicatesParser = allDupsParser <|> oneEachDupsParser <|> allRepeatedParser
    where
        allDupsParser = flag NoDups AllDups ( short 'D' )
        oneEachDupsParser = flag' OneEachDups ( short 'd' )


allRepeatedParser :: Parser DuplicateHandling
allRepeatedParser = defaultRepeatedParser <|> noneRepeatedParser <|> prependParser <|> separateParser
    where
        defaultRepeatedParser = flag' AllDups ( long "long-repeated" ) --default if no method
        noneRepeatedParser = flag' AllDups ( long "long-repeated=none" )
        prependParser = flag' (AllRepeated Prepend) ( long "long-repeated=prepend" )
        separateParser = flag' (AllRepeated Separate) (long "long-repeated=separate" )

skipFieldsParser :: Parser Int
skipFieldsParser = option auto ( short 'f' <> long "skip-fields" <> value 0 <> help "TODO avoid compariong the first N fields" )

groupHandlingParser :: Parser GroupHandling
groupHandlingParser = defaultGroupHandlingParser <|> separateGroupsParser <|> prependGroupsParser <|> appendGroupsParser <|> bothGroupsParser
    where
        defaultGroupHandlingParser = flag NoGroups SeparateGroups ( long "group" )
        separateGroupsParser = flag' SeparateGroups ( long "group=separate" )
        prependGroupsParser = flag' PrependGroups ( long "group=prepend" )
        appendGroupsParser = flag' AppendGroups ( long "group=append" )
        bothGroupsParser = flag' BothGroups ( long "group=both" )


ignoreCaseParser :: Parser Bool
ignoreCaseParser = switch ( short 'i' <> long "ignore-case" <> help "TODO ignore differences in case when comparing" )

skipCharsParser :: Parser Int
skipCharsParser = option auto ( value 0 <> short 's' <> long "skip-chars" <> help "TODO avoid comparing the first N characters" )

uniqueOnlyParser :: Parser Bool
uniqueOnlyParser = switch ( short 'u' <> long "unique" <> help "TODO Only print unique lines" )

nullTerminatedParser :: Parser Bool
nullTerminatedParser = switch ( short 'z' <> long "zero-temrinated" <> help "TODO line delimiter is NUL, not lewline")

checkCharsParser :: Parser (Maybe Int)
checkCharsParser = optional ( option auto ( short 'w' <> long "check-chars" <> help "TODO compare no more than N characters in lines"))

data UniqArgs = UniqArgs {
                    appOpts :: UniqOpts,
                    input :: Maybe FilePath,
                    output :: Maybe FilePath
                } deriving Show

argsParser :: Parser UniqArgs
argsParser = UniqArgs
          <$> optsParser
          <*> optional ( argument str (metavar "INPUT"))
          <*> optional ( argument str (metavar "OUTPUT"))

optsParse :: ParserInfo UniqArgs
optsParse =
    info (helper <*> versionOption <*> argsParser)
      ( fullDesc <> header "uniq" <>
        progDesc "Haskell coreutils by George Takumi Crary")
    where
        versionOption :: Parser (a -> a)
        versionOption = infoOption "1.0.0" (short 'v' <> long "version" <>
                                            help "Show version")

main :: IO ()
main = do
    args <- execParser optsParse
    inHandle <- do case input args of
                        Just fp -> openFile fp ReadMode
                        Nothing -> return stdin
    outHandle <- do case output args of
                        Just fp -> openFile fp WriteMode
                        Nothing -> return stdout

    main' (appOpts args) inHandle outHandle
    exitSuccess


tossSeqRepeat :: Eq a => [a] -> [a]
tossSeqRepeat (x:y:xs) = if x == y
                      then tossSeqRepeat $ x:xs
                      else x: tossSeqRepeat (y:xs)

tossSeqRepeat x = x

main' :: UniqOpts -> Handle -> Handle -> IO ()
main' defaultOpts inHandle outHandle = do
    mergedLines <- tossSeqRepeat . lines <$> hGetContents' inHandle
    forM_ mergedLines $ \line -> do
        hPutStrLn outHandle line

main' opts inHandle outHandle = undefined
