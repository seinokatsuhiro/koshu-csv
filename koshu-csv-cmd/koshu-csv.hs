{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert CSV to Koshucode

module Main (main) where

import qualified Text.CSV                        as Csv
import qualified Koshucode.Baala.System          as Z
import qualified Koshucode.Baala.DataPlus        as K
import qualified Koshucode.Baala.Syntax.Pattern  as P
import qualified Paths_koshu_csv_cmd             as Ver


-- --------------------------------------------  Main

versionString :: String
versionString = "Koshucode CSV Converter " ++ Z.showVersion Ver.version

usageHeader :: [String]
usageHeader =
    [ "DESCRIPTION"
    , "  Convert CSV to Koshucode"
    , ""
    , "USAGE"
    , "  koshu-csv [OPTION] FILE.csv ..."
    , ""
    ]

-- | Entry point of the @koshu-csv@ command.
main :: IO ()
main = do cmd <- Z.parseCommand options
          p   <- initPara cmd
          mainPara p

mainPara :: Para -> IO ()
mainPara p@Para { paraHelp = help, paraVersion = version }
    | help       = Z.printHelp usageHeader options
    | version    = putStrLn versionString
    | otherwise  = do ls <- convertPrintCsvFiles p
                      let output = K.mixBlock ls
                      case paraOutput p of
                        Nothing   -> K.putMix K.crlfBreak output
                        Just path -> K.writeMix K.crlfBreak path output

asText :: K.Tx -> K.Tx
asText = id


-- --------------------------------------------  Parameter

options :: [Z.Option]
options =
    [ Z.help
    , Z.version

    , Z.flag ""  ["emacs-mode"]         "File: Include Emacs mode comment"
    , Z.req  ""  ["include"] "FILE.k"   "File: Include file"
    , Z.req  ""  ["license"] "FILE"     "File: Include license file"
    , Z.req  "o" ["output"] "FILE.k"    "File: Output file"

    , Z.req  "l" ["layout"] "FILE"      "Data: Layout file"
    , Z.req  ""  ["drop"] "NUM"         "Data: Number of dropping lines"

    , Z.flag ""  ["to-dec"]             "Term: Convert decimal number if passible"
    , Z.flag ""  ["to-empty"]           "Term: Convert empty string to ()"
    , Z.flag ""  ["trim", "trim-both"]  "Term: Trim spaces"
    , Z.flag ""  ["trim-begin"]         "Term: Trim spaces beginning of value"
    , Z.flag ""  ["trim-end"]           "Term: Trim spaces end of value"
    , Z.opt  ""  ["seq"] "/TERM"        "Term: Add sequential number term (default /seq)"
    ]

-- | Command parameter.
data Para = Para
    { paraLayout     :: CsvLayout
    , paraEmacsMode  :: Bool              -- ^ Inlucde emacs mode
    , paraIncludes   :: [String]          -- ^ Inlucde lines
    , paraLicenses   :: [String]          -- ^ License lines
    , paraInput      :: [FilePath]        -- ^ Input CSV files
    , paraOutput     :: Maybe FilePath    -- ^ Output Koshucode file

    , paraHelp       :: Bool
    , paraVersion    :: Bool
    } deriving (Show, Eq, Ord)

-- | Initialize parameter.
initPara :: Z.Parsed -> IO Para
initPara (Left errs) = error $ unwords errs
initPara (Right (z, args)) =
    do comment <- readFilesLines $ req "include"
       license <- readFilesLines $ req "license"
       lay  <- readLayoutFiles $ req "layout"
       let lay' = lay { csvSeq           = case opt "seq" "/seq" of
                                             Nothing -> csvSeq lay
                                             Just n  -> Just $ K.toTermName n
                      , csvDrop          = case reql "drop" of
                                             Nothing -> csvDrop lay
                                             Just n  -> K.fromMaybe 1 $ K.tInt n
                      , csvGlobalEmpty   = flag "to-empty"  || csvGlobalEmpty   lay
                      , csvGlobalDecimal = flag "to-dec"    || csvGlobalDecimal lay }
       return $ Para { paraLayout     = case trim of
                                          Nothing -> lay'
                                          Just t  -> lay' { csvGlobalTrim = t }
                     , paraEmacsMode  = flag "emacs-mode"
                     , paraIncludes   = comment
                     , paraLicenses   = license
                     , paraInput      = args
                     , paraOutput     = reql "output"

                     , paraHelp       = flag "help"
                     , paraVersion    = flag "version" }
    where
      flag  = Z.getFlag    z
      req   = Z.getReq     z
      reql  = Z.getReqLast z
      opt   = Z.getOptLast z

      trim | flag "trim"        = Just (True, True)
           | flag "trim-both"   = Just (True, True)
           | flag "trim-left"   = Just (True, False)
           | flag "trim-right"  = Just (False, True)
           | otherwise          = Nothing

-- | Read multiple files.
readFilesLines :: [FilePath] -> IO [String]
readFilesLines paths =
    do lsList <- mapM readFileLines paths
       return $ concat lsList

-- | Read file and split lines.
readFileLines :: FilePath -> IO [String]
readFileLines path =
    do file <- K.readBzFile path
       bz   <- K.abortLeft $ K.bzFileContent file
       let ls = K.bzString <$> K.bzLines bz
       return ls


-- --------------------------------------------  Layout

data CsvLayout =
    CsvLayout
    { csvClass          :: K.JudgeClass       -- ^ Judgement class
    , csvSeq            :: Maybe K.TermName   -- ^ Add sequential number term
    , csvTerms          :: [CsvTerm]          -- ^ Term specifications
    , csvDrop           :: Int                -- ^ Number of dropping lines
    , csvGlobalTrim     :: (Bool, Bool)       -- ^ Trim value
    , csvGlobalEmpty    :: Bool               -- ^ Convert empty string
    , csvGlobalDecimal  :: Bool               -- ^ Convert decimal number
    } deriving (Show, Eq, Ord)

instance K.Default CsvLayout where
    def = CsvLayout { csvClass         = "CSV"
                    , csvSeq           = Nothing
                    , csvTerms         = []
                    , csvDrop          = 0
                    , csvGlobalTrim    = (False, False)
                    , csvGlobalEmpty   = False
                    , csvGlobalDecimal = False }

data CsvTerm =
    CsvTerm { csvTermName :: K.TermName
            , csvEmpty    :: Bool
            , csvDecimal  :: Bool
            } deriving (Show, Eq, Ord)

instance K.ToTermName CsvTerm where
    toTermName = csvTermName
    toTermNameOrd _ = K.toTermName . csvTermName

csvTerm :: (K.ToTermName n) => n -> CsvTerm
csvTerm n = CsvTerm { csvTermName = K.toTermName n
                    , csvEmpty    = False
                    , csvDecimal  = False }

finishLayout :: K.Map CsvLayout
finishLayout lay@CsvLayout { csvTerms = ts} =
    lay { csvTerms = if null ts
                     then numericTerms
                     else reverse ts }

-- | List of numeric-named terms.
numericTerms :: [CsvTerm]
numericTerms = csvTerm <$> K.ints 1

readLayoutFiles :: [FilePath] -> IO CsvLayout
readLayoutFiles = loop K.def where
    loop lay [] = return $ finishLayout lay
    loop lay (f:fs) = do lay' <- readLayoutFile f lay
                         loop lay' fs

readLayoutFile :: FilePath -> CsvLayout -> IO CsvLayout
readLayoutFile path lay =
    do ls' <- K.readClauseTokens path
       ls  <- K.abortLeft ls'
       K.abortLeft $ layoutClauses ls lay

layoutClauses :: [[K.Token]] -> K.AbMap CsvLayout
layoutClauses [] lay = return lay
layoutClauses (l:ls) lay =
    do lay' <- K.abortable "clause" l $ layoutClause l lay
       layoutClauses ls lay'

layoutClause :: [K.Token] -> K.AbMap CsvLayout
layoutClause (P.Term n : ks) lay =
    do term <- csvTermKeyword ks $ csvTerm n
       Right $ lay { csvTerms = term : csvTerms lay }
layoutClause [P.TBar "|==", P.TRaw cl] lay =
    Right $ lay { csvClass = K.tString cl }
layoutClause (P.TRaw "drop" : ts) lay =
    case ts of
      [P.TRaw n] -> case K.tInt n of
                      Just i  -> Right $ lay { csvDrop = i }
                      Nothing -> Left $ K.abortBecause $ asText "Expect integer"
      _                       -> Left $ K.abortBecause $ asText "Expect integer"
layoutClause [P.TRaw "seq"]           lay = Right $ lay { csvSeq = Just $ K.toTermName $ asText "seq" }
layoutClause [P.TRaw "seq", P.Term n] lay = Right $ lay { csvSeq = Just $ K.toTermName n }
layoutClause [P.TRaw k] lay
    | k == "trim"        = Right $ lay { csvGlobalTrim    = (True,  True)  }
    | k == "trim-both"   = Right $ lay { csvGlobalTrim    = (True,  True)  }
    | k == "trim-begin"  = Right $ lay { csvGlobalTrim    = (True,  False) }
    | k == "trim-end"    = Right $ lay { csvGlobalTrim    = (False, True)  }
    | k == "to-empty"    = Right $ lay { csvGlobalEmpty   = True }
    | k == "to-dec"      = Right $ lay { csvGlobalDecimal = True }
layoutClause _ _ = Left $ K.abortBecause $ asText "unknown clause"

csvTermKeyword :: [K.Token] -> K.AbMap CsvTerm
csvTermKeyword = loop where
    loop (P.TRaw "to-dec"   : ks) term = loop ks $ term { csvDecimal = True }
    loop (P.TRaw "to-empty" : ks) term = loop ks $ term { csvEmpty   = True }
    loop (t : _) _ = K.abortable "keyword" t $ Left $ K.abortBecause $ asText "unknown keyword"
    loop [] term = Right term

-- --------------------------------------------  Convert CSV file

convertPrintCsvFiles :: Para -> IO [K.MixText]
convertPrintCsvFiles p =
    do ls <- convertPrintCsvFile (convertCsvString p) `mapM` paraInput p
       return $ appendBlock $ mode' : include' : license' : ls
    where
      mode'     | paraEmacsMode p  = mixes [asText "** -*- koshu -*-"]
                | otherwise        = []

      include   = paraIncludes p
      include'  | null include     = []
                | otherwise        = mixes include

      license   = paraLicenses p
      license'  | null license     = []
                | otherwise        = mixes
                                     ( ["=== license", ""]
                                       ++ map ("  " ++) license
                                       ++ ["", "=== rel"] )

appendBlock :: [[K.MixText]] -> [K.MixText]
appendBlock [] = []
appendBlock ([] : bs) = appendBlock bs
appendBlock [b]       = [K.mixLines b]
appendBlock (b : bs)  = K.mixLines b : K.mixHard : appendBlock bs

-- | Convert and print CSV file to Koshucode.
convertPrintCsvFile :: (FilePath -> String -> [K.MixText]) -> FilePath -> IO [K.MixText]
convertPrintCsvFile f path =
    do file <- K.readBzFile path
       bz   <- K.abortLeft $ K.bzFileContent file
       return $ mixes $ f path $ K.bzString bz

-- | Convert CSV string.
convertCsvString :: Para -> FilePath -> String -> [K.MixText]
convertCsvString p path content = K.mixEncode <$> parseConvertCsv p path content

mixes :: (Functor f, K.Mix a) => f a -> f K.MixText
mixes = fmap K.mix


-- --------------------------------------------  Convert CSV string

-- | Parse and convert CSV.
parseConvertCsv :: Para -> FilePath -> String -> [K.JudgeC]
parseConvertCsv p path content =
    case Csv.parseCSV path content of
      Left a    -> error $ show a
      Right csv -> convertCsv p csv

-- | Convert CSV.
convertCsv :: Para -> [Csv.Record] -> [K.JudgeC]
convertCsv p csv = zipWith judge (K.ints 1) csv' where
    lay     = paraLayout p
    judge   = csvJudge lay
    csv'    = omitEmptyLines
               $ map (map $ trimValue $ csvGlobalTrim $ paraLayout p)
               $ drop (csvDrop lay) csv

-- | Omit empty lines.
omitEmptyLines :: K.Map [Csv.Record]
omitEmptyLines = K.omit (== [""])

-- | Trim begin or end of value.
trimValue :: (Bool, Bool) -> K.Map String
trimValue (False , False) = id
trimValue (True  , True)  = K.trimBoth
trimValue (True  , False) = K.trimBegin
trimValue (False , True)  = K.trimEnd

-- | Create judgement.
csvJudge :: CsvLayout -> Int -> Csv.Record -> K.JudgeC
csvJudge CsvLayout {..} n values =
    case csvSeq of
      Nothing   -> K.affirm csvClass ts'
      Just name -> K.affirm csvClass $ (name, K.pInt n) : ts'
    where
      ts' = K.cutEmpty ts
      ts  = termC csvGlobalEmpty csvGlobalDecimal <$> zip csvTerms values

-- | Create term content.
termC :: Bool -> Bool -> (CsvTerm, String) -> K.TermC
termC empty dec (t, s) = K.term t c where
    c      = pEmptyDecText empty' dec' s
    empty' = empty || csvEmpty t
    dec'   = dec   || csvDecimal t

pEmptyDecText :: Bool -> Bool -> String -> K.Content
pEmptyDecText True _ ""  = K.empty
pEmptyDecText _ False s  = K.pText s
pEmptyDecText _ True  s  = case K.decodeDecimal s of
                             Right d -> K.pDec d
                             _       -> K.pText s
