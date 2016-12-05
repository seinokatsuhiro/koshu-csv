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
                      let output = unlines ls
                      case paraOutput p of
                        Nothing   -> putStr output
                        Just path -> writeFile path output


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
    , Z.opt  ""  ["body"] "NUM"         "Data: Line number at body starts (default 1)"

    , Z.flag ""  ["to-decimal"]         "Term: Convert decimal number if passible"
    , Z.flag ""  ["to-empty"]           "Term: Convert empty string to ()"
    , Z.flag ""  ["trim", "trim-both"]  "Term: Trim spaces"
    , Z.flag ""  ["trim-begin"]         "Term: Trim spaces beginning of value"
    , Z.flag ""  ["trim-end"]           "Term: Trim spaces end of value"
    , Z.opt  ""  ["seq"] "/TERM"        "Term: Add sequential number term (default /seq)"
    ]

-- | Command parameter.
data Para = Para
    { paraLayout     :: CsvLayout
    , paraBody       :: Int               -- ^ Line number at body starts
    , paraSeq        :: Maybe K.TermName  -- ^ Add sequential number term
    , paraEmacsMode  :: Bool              -- ^ Inlucde emacs mode
    , paraIncludes   :: [String]          -- ^ Inlucde lines
    , paraLicenses   :: [String]          -- ^ License lines
    , paraEmpty      :: Bool              -- ^ Convert empty string
    , paraInput      :: [FilePath]        -- ^ Input CSV files
    , paraOutput     :: Maybe FilePath    -- ^ Output Koshucode file

    , paraHelp       :: Bool
    , paraVersion    :: Bool
    } deriving (Show, Eq, Ord)

-- | Initialize parameter.
initPara :: Z.Parsed -> IO Para
initPara (Left errs) = error $ unwords errs
initPara (Right (z, args)) =
    do comment <- readTextFiles $ req "include"
       license <- readTextFiles $ req "license"
       layout  <- readLayoutFiles $ req "layout"
       let layout' = layout { csvGlobalDecimal = flag "to-decimal" }
       return $ Para { paraLayout     = case trim of
                                          Nothing -> layout'
                                          Just t  -> layout' { csvGlobalTrim = t }
                     , paraBody       = body
                     , paraSeq        = K.toTermName <$> opt "seq" "/seq"
                     , paraEmacsMode  = flag "emacs-mode"
                     , paraIncludes   = comment
                     , paraLicenses   = license
                     , paraEmpty      = flag "to-empty"
                     , paraInput      = args
                     , paraOutput     = reql "output"

                     , paraHelp       = flag "help"
                     , paraVersion    = flag "version" }
    where
      flag  = Z.getFlag    z
      req   = Z.getReq     z
      reql  = Z.getReqLast z
      opt   = Z.getOptLast z

      body = K.fromMaybe 1 $ do
               n <- opt "body" "1"
               K.stringInt n

      trim | flag "trim"        = Just (True, True)
           | flag "trim-both"   = Just (True, True)
           | flag "trim-left"   = Just (True, False)
           | flag "trim-right"  = Just (False, True)
           | otherwise          = Nothing

readTextFiles :: [FilePath] -> IO [String]
readTextFiles paths =
    do contents <- mapM readFile paths
       return $ concatMap lines contents


-- --------------------------------------------  Layout

data CsvLayout =
    CsvLayout
    { csvClass          :: K.JudgeClass
    , csvTerms          :: [CsvTerm]
    , csvGlobalTrim     :: (Bool, Bool)
    , csvGlobalDecimal  :: Bool
    } deriving (Show, Eq, Ord)

instance K.Default CsvLayout where
    def = CsvLayout { csvClass          = "CSV"
                    , csvTerms         = []
                    , csvGlobalTrim    = (False, False)
                    , csvGlobalDecimal = False }

data CsvTerm =
    CsvTerm { csvTermName :: K.TermName
            , csvDecimal  :: Bool
            } deriving (Show, Eq, Ord)

instance K.ToTermName CsvTerm where
    toTermName = csvTermName

csvTerm :: (K.ToTermName n) => n -> CsvTerm
csvTerm n = CsvTerm { csvTermName = K.toTermName n
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
    do ls <- K.readClauses path
       K.abortLeft $ layoutClauses ls lay

layoutClauses :: [K.TokenClause] -> K.AbMap CsvLayout
layoutClauses [] lay = return lay
layoutClauses (l:ls) lay =
    do lay' <- K.abortable "clause" l $ layoutClause (K.clauseTokens l) lay
       layoutClauses ls lay'

layoutClause :: [K.Token] -> K.AbMap CsvLayout
layoutClause (P.Term n : ks) lay =
    do term <- csvTermKeyword ks $ csvTerm n
       Right $ lay { csvTerms = term : csvTerms lay }
layoutClause [P.TBar "|==", P.TRaw cl] lay =
    Right $ lay { csvClass = cl }
layoutClause [P.TRaw k] lay
    | k == "trim"        = Right $ lay { csvGlobalTrim = (True,  True)  }
    | k == "trim-both"   = Right $ lay { csvGlobalTrim = (True,  True)  }
    | k == "trim-begin"  = Right $ lay { csvGlobalTrim = (True,  False) }
    | k == "trim-end"    = Right $ lay { csvGlobalTrim = (False, True)  }
layoutClause _ _ = Left $ K.abortBecause "unknown clause"

csvTermKeyword :: [K.Token] -> K.AbMap CsvTerm
csvTermKeyword = loop where
    loop (P.TRaw "to-dec" : ks) term = loop ks $ term { csvDecimal = True }
    loop (t : _) _ = K.abortable "keyword" t $ Left $ K.abortBecause "unknown keyword"
    loop [] term = Right term

-- --------------------------------------------  Convert CSV file

convertPrintCsvFiles :: Para -> IO [String]
convertPrintCsvFiles p =
    do ls <- convertPrintCsvFile (convertCsvString p) `mapM` paraInput p
       return $ appendBlock [mode', include', license', concat ls]
    where
      mode'     | paraEmacsMode p  = ["** -*- koshu -*-"]
                | otherwise        = []

      include   = paraIncludes p
      include'  | null include     = []
                | otherwise        = include

      license   = paraLicenses p
      license'  | null license     = []
                | otherwise        = ["=== license", ""]
                                     ++ map ("  " ++) license
                                     ++ ["", "=== rel"]

appendBlock :: [[String]] -> [String]
appendBlock [] = []
appendBlock ([] : bs) = appendBlock bs
appendBlock [b]       = b
appendBlock (b : bs)  = b ++ [""] ++ appendBlock bs

-- | Convert and print CSV file to Koshucode.
convertPrintCsvFile :: K.ManyMap String -> FilePath -> IO [String]
convertPrintCsvFile f path =
    do content <- readFile path
       return $ f content

-- | Convert CSV string.
convertCsvString :: Para -> K.ManyMap String
convertCsvString p s = K.plainEncode <$> parseConvertCsv p s


-- --------------------------------------------  Convert CSV string

-- | Parse and convert CSV.
parseConvertCsv :: Para -> String -> [K.JudgeC]
parseConvertCsv p s =
    case Csv.parseCSV "<stdin>" s of
      Left a    -> error $ show a
      Right csv -> convertCsv p csv

-- | Convert CSV.
convertCsv :: Para -> [Csv.Record] -> [K.JudgeC]
convertCsv p csv = zipWith judge (K.ints 1) csv' where
    judge = csvJudge (paraSeq p) (paraEmpty p, csvGlobalDecimal $ paraLayout p) (paraLayout p)
    csv'  = omitEmptyLines
              $ K.map2 (trimValue $ csvGlobalTrim $ paraLayout p)
              $ dropHead (paraBody p) csv

-- | Drop heading
dropHead :: Int -> K.Map [Csv.Record]
dropHead n = drop (n - 1)

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
csvJudge :: Maybe K.TermName -> (Bool, Bool) -> CsvLayout -> Int -> Csv.Record -> K.JudgeC
csvJudge maybeSeq to CsvLayout { csvClass = cl, csvTerms = ns } n values =
    case maybeSeq of
      Nothing   -> K.affirm cl ts
      Just name -> K.affirm cl $ (name, K.pInt n) : ts
    where
      ts  = termC to <$> zip ns values

-- | Create term content.
termC :: (Bool, Bool) -> (CsvTerm, String) -> K.TermC
termC (True, _) (t, "") = K.term t K.empty
termC (_, dec) (t, s)
    | dec           = K.term t $ pDecText s
    | csvDecimal t  = K.term t $ pDecText s
    | otherwise     = K.term t $ K.pText s

pDecText :: String -> K.Content
pDecText s = case K.decodeDecimal s of
               Right d -> K.pDec d
               _       -> K.pText s
