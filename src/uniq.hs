module Main where

import System.IO
import System.Exit (exitSuccess)
import Options.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
                                                --NoSeperation for allDups
data DuplicateHandling = NoDups | OneEachDups | AllDups | AllRepeated SeperationHandling | Grouped GroupHandling | UniqueHandling
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
                    ignoreCase       :: Bool,               -- -i --ignore-case
                    skipChars        :: Int,                -- -s --skip-chars=N
                    nullTerminated   :: Bool,               -- -z --zero-terminated
                    checkChars       :: Maybe Int           -- -w --check-chars=N
                }deriving Show

defaultOpts :: UniqOpts
defaultOpts = UniqOpts False NoDups 0 False 0 False Nothing

optsParser :: Parser UniqOpts
optsParser = UniqOpts
            <$> outputCountParser
            <*> outputDuplicatesParser
            <*> skipFieldsParser
            <*> ignoreCaseParser
            <*> skipCharsParser
            <*> nullTerminatedParser
            <*> checkCharsParser

outputCountParser :: Parser Bool
outputCountParser = switch ( short 'c' <> long "count" <> help "TODO prefix lines by the number of occurences" )

-- -d -D --all-Repeated[GROUPS]
outputDuplicatesParser :: Parser DuplicateHandling
outputDuplicatesParser = allDupsParser <|> oneEachDupsParser <|> allRepeatedParser <|> groupHandlingParser <|> uniqueOnlyParser
    where
        allDupsParser = flag NoDups AllDups ( short 'D' )
        oneEachDupsParser = flag' OneEachDups ( short 'd' )
        uniqueOnlyParser = flag' UniqueHandling ( short 'u' <> long "unique" <> help "TODO Only print unique lines" )


allRepeatedParser :: Parser DuplicateHandling
allRepeatedParser = defaultRepeatedParser <|> noneRepeatedParser <|> prependParser <|> separateParser
    where
        defaultRepeatedParser = flag' AllDups ( long "long-repeated" ) --default if no method
        noneRepeatedParser = flag' AllDups ( long "long-repeated=none" )
        prependParser = flag' (AllRepeated Prepend) ( long "long-repeated=prepend" )
        separateParser = flag' (AllRepeated Separate) (long "long-repeated=separate" )


groupHandlingParser :: Parser DuplicateHandling
groupHandlingParser = separateGroupsParser <|> prependGroupsParser <|> appendGroupsParser <|> bothGroupsParser
    where
        separateGroupsParser = flag' (Grouped SeparateGroups) ( long "group=separate" )
        prependGroupsParser  = flag' (Grouped PrependGroups) ( long "group=prepend" )
        appendGroupsParser   = flag' (Grouped AppendGroups) ( long "group=append" )
        bothGroupsParser     = flag' (Grouped BothGroups) ( long "group=both" )


skipFieldsParser :: Parser Int
skipFieldsParser = option auto ( short 'f' <> long "skip-fields" <> value 0 <> help "TODO avoid compariong the first N fields" )

ignoreCaseParser :: Parser Bool
ignoreCaseParser = switch ( short 'i' <> long "ignore-case" <> help "TODO ignore differences in case when comparing" )

skipCharsParser :: Parser Int
skipCharsParser = option auto ( value 0 <> short 's' <> long "skip-chars" <> help "TODO avoid comparing the first N characters" )


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

skipNfields :: Int -> String -> String
skipNfields n s = (iterate dropField s) !! n
--TODO see if gnu uniq drops leading white space after dropping the fields

dropField :: String -> String
dropField = dropWhile isAlphaNum . dropWhile isSpace

-- This should always be used for comparing lines
limitedString :: UniqOpts -> String -> String
limitedString opt = igf . checkLimit . char_skipper . field_skipper
    where
        igf = if ignoreCase opt
              then map toLower
              else id
        field_skipper = skipNfields $ skipFields opt
        char_skipper = drop $ skipChars opt
        checkLimit = case checkChars opt of
                        Nothing -> id
                        Just count -> if count == 0
                                      then const []
                                      else take count


-- TODO map file lines into Map limitedString Occurance then handle printing by d/D/u/repeated/grouping

-- Handles -i -s -w flags TODO look into skip-fields
desiredCompare :: UniqOpts -> String -> String -> Ordering
desiredCompare defaultOpts x y = compare x y
desiredCompare opt xs ys = compare (limiter xs) (limiter ys)
    where
        limiter = limitedString opt

type LineNumber = Int

data Occurance = Unique LineNumber | Duplicated [LineNumber]
    deriving Show

instance Semigroup Occurance where
    (<>) (Unique l) (Unique r)           = Duplicated [l,r]
    (<>) (Unique l) (Unique r)           = Duplicated [l,r]
    (<>) (Unique l) (Duplicated xs)      = Duplicated (l:xs)
    (<>) (Duplicated xs) (Unique l)      = Duplicated (l:xs)
    (<>) (Duplicated xs) (Duplicated ys) = Duplicated (xs++ys)

lineOccurances :: UniqOpts -> [String] -> Map.Map String Occurance
lineOccurances opts xs = Map.fromListWith (<>) $ toUniqueSingle <$> zip xs [1..]
    where
        toUniqueSingle = (\(s,ln) -> ((limitedString opts) s, Unique ln))

filterUniqueOccs :: [Occurance] -> [Occurance]
filterUniqueOccs xs = [x | x@(Unique {}) <- xs]

filterDuplicatedOccs :: [Occurance] -> [Occurance]
filterDuplicatedOccs xs = [x | x@(Duplicated {}) <- xs]

addOccurance :: Occurance -> LineNumber -> Occurance
addOccurance (Unique fstln) newln = Duplicated [fstln,newln]
addOccurance (Duplicated xs) newln = Duplicated $ newln:xs

main' :: UniqOpts -> Handle -> Handle -> IO ()
main' defaultOpts inHandle outHandle = do
    mergedLines <- tossSeqRepeat . lines <$> hGetContents' inHandle
    forM_ mergedLines $ \line -> do
        hPutStrLn outHandle line

main' opts inHandle outHandle = undefined
