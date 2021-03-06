module Main where

import System.IO
import System.Exit (exitSuccess)
import Options.Applicative
import Control.Monad
import Data.Char
import Data.Bifunctor

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

data DuplicateHandling = NoAdjacentDups | -- default
                         OneEachDups | -- d
                         AllDups | -- D
                         AllRepeated SeperationHandling | --all-repeated
                         NoDups -- u

    deriving Show

--Maybe these clash
data SeperationHandling = Prepend | Separate
    deriving Show

data UniqOpts = UniqOpts {                                  -- Corresponding GNU Uniq flags
                    style            :: DuplicateHandling,  -- -d --all-repeated
                    ignoreCase       :: Bool,               -- -i --ignore-case
                    skipFields       :: Int,                -- -f --skip-fields=N
                    checkChars       :: Maybe Int,          -- -w --check-chars=N
                    skipChars        :: Int,                -- -s --skip-chars=N
                    outputCount      :: Bool,               -- -c --count
                    nullTerminated   :: Bool                -- -z --zero-terminated
                }deriving Show

-- defaultOpts :: UniqOpts
-- defaultOpts = UniqOpts False NoAdjacentDups 0 False 0 False Nothing

optsParser :: Parser UniqOpts
optsParser = UniqOpts
            <$> outputDuplicatesParser
            <*> ignoreCaseParser
            <*> skipFieldsParser
            <*> checkCharsParser
            <*> skipCharsParser
            <*> outputCountParser
            <*> nullTerminatedParser


outputCountParser :: Parser Bool
outputCountParser = switch ( short 'c' <> long "count" <> help "TODO prefix lines by the number of occurences" )

-- -d -D --all-Repeated[GROUPS]
outputDuplicatesParser :: Parser DuplicateHandling
outputDuplicatesParser = allDupsParser <|> oneEachDupsParser <|> allRepeatedParser <|> noDupesParser
    where
        allDupsParser = flag NoDups AllDups ( short 'D' )
        oneEachDupsParser = flag' OneEachDups ( short 'd' )
        noDupesParser = flag' NoDups ( short 'u' <> long "unique" <> help "TODO Only print unique lines if no duplicate handling is involved" )


allRepeatedParser :: Parser DuplicateHandling
allRepeatedParser = defaultRepeatedParser <|> noneRepeatedParser <|> prependParser <|> separateParser
    where
        defaultRepeatedParser = flag' AllDups ( long "long-repeated" ) --default if no method
        noneRepeatedParser = flag' AllDups ( long "long-repeated=none" )
        prependParser = flag' (AllRepeated Prepend) ( long "long-repeated=prepend" )
        separateParser = flag' (AllRepeated Separate) (long "long-repeated=separate" )


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

    content <- hGetContents' inHandle
    main' (appOpts args) content outHandle
    exitSuccess


tossSeqRepeat :: Eq a => [a] -> [a]
tossSeqRepeat (x:y:xs) = if x == y
                      then tossSeqRepeat $ x:xs
                      else x: tossSeqRepeat (y:xs)

tossSeqRepeat x = x

skipNfields :: Int -> String -> String
skipNfields n s = iterate dropField s !! n
--TODO see if gnu uniq drops leading white space after dropping the fields

dropField :: String -> String
dropField = dropWhile isAlphaNum . dropWhile isSpace

-- This should always be used for comparing lines
-- NOTE. maybe this should be new typed to distingush its a limited view of the string
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
desiredCompare (UniqOpts _ False 0 Nothing 0 _ _) x y = compare x y
desiredCompare opt xs ys = compare (limiter xs) (limiter ys)
    where
        limiter = limitedString opt

type LineNumber = Int

data Occurance = Unique LineNumber | Duplicated [LineNumber]
    deriving Show

instance Semigroup Occurance where
    (<>) (Unique l) (Unique r)           = Duplicated [l,r]
    (<>) (Unique l) (Duplicated xs)      = Duplicated (l:xs)
    (<>) (Duplicated xs) (Unique l)      = Duplicated (l:xs)
    (<>) (Duplicated xs) (Duplicated ys) = Duplicated (xs++ys)

lineOccurances :: UniqOpts -> [String] -> Map.Map String Occurance
lineOccurances opts xs = Map.fromListWith (<>) $ toUniqueSingle <$> zip xs [1..]
    where
        toUniqueSingle = bimap (limitedString opts) Unique

filterUniqueOccs :: [Occurance] -> [Occurance]
filterUniqueOccs xs = [x | x@Unique {} <- xs]

filterDuplicatedOccs :: [Occurance] -> [Occurance]
filterDuplicatedOccs xs = [x | x@Duplicated {} <- xs]

addOccurance :: Occurance -> LineNumber -> Occurance
addOccurance (Unique fstln) newln = Duplicated [fstln,newln]
addOccurance (Duplicated xs) newln = Duplicated $ newln:xs

handleOutputFormat :: UniqOpts -> Map.Map String Occurance -> Handle -> String -> IO ()
handleOutputFormat opts occmap handle line = do
    when (outputCount opts)
       $ case occmap ! limitedString opts line of
            Unique _ -> hPutStr handle "1 "
            Duplicated xs -> hPutStr handle $ show (length xs) ++ " "

    hPutStr handle line

    if nullTerminated opts
    then hPutStr handle "\0"
    else hPutStr handle "\n"

pUniqueOcc :: Ord k => p -> Map.Map k Occurance -> (p -> k) -> Bool
pUniqueOcc line occmap limiter = case occurance of
                                    Unique _ -> True
                                    Duplicated _-> False
     where occurance = occmap ! limiter line

pDuplicateOcc :: Ord k => p -> Map.Map k Occurance -> (p -> k) -> Bool
pDuplicateOcc line occmap limiter = case occurance of
                                    Unique _ -> False
                                    Duplicated _-> True
     where occurance = occmap ! limiter line

main' :: UniqOpts -> String -> Handle -> IO ()
main' opts@(UniqOpts NoAdjacentDups _ _ _ _ _ _) content outHandle = do
    let flines = lines content
    let occmap = lineOccurances opts flines

    mapM_ (handleOutputFormat opts occmap outHandle) . tossSeqRepeat $ flines

main' opts@(UniqOpts OneEachDups _ _ _ _ _ _) content outHandle = do
    let flines = lines content
    let occmap = lineOccurances opts flines
    let limiter = limitedString opts

    forM_ (zip flines [1..]) (\(line,line_number) -> do
       case occmap ! limiter line of
        Unique _ -> handleOutputFormat opts occmap outHandle line
        Duplicated xs -> when (head xs == line_number)
                            $ handleOutputFormat opts occmap outHandle line
        )

main' opts@(UniqOpts ustyle _ _ _ _ _ _ ) content outHandle = do
    let flines = lines content
    let occmap = lineOccurances opts flines
    let limiter = limitedString opts

    forM_ flines (\line -> do
        let pf = handleOutputFormat opts occmap outHandle line
        case ustyle of
            NoDups          -> when(pUniqueOcc line occmap limiter) pf
            AllDups         -> when (pDuplicateOcc line occmap limiter) pf
            _               -> return () --should be handled already
        )

-- main' opts@( content outHandle = do
--    let uniquenessMap = lineOccurances opts . lines $ content -- keyed by the limitedView of the string
--    return ()
