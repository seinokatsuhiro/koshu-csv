{-# OPTIONS_GHC -Wall #-}

-- | Convert CSV to Koshucode

module Main (main, JudgeType (..)) where

import qualified Text.CSV                        as Csv
import qualified Koshucode.Baala.DataPlus        as K
import qualified Koshucode.Baala.System          as Z
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

options :: [Z.Option]
options =
    [ Z.help
    , Z.version

    , Z.flag ""  ["emacs-mode"]         "File: Include Emacs mode comment"
    , Z.req  ""  ["include"] "FILE.k"   "File: Include file"
    , Z.req  ""  ["license"] "FILE"     "File: Include license file"

    , Z.opt  ""  ["body"] "NUM"         "Data: Line number at body starts (default 1)"

    , Z.flag ""  ["to-decimal"]         "Term: Convert decimal number if passible"
    , Z.flag ""  ["to-empty"]           "Term: Convert empty string to ()"
    , Z.flag ""  ["trim", "trim-both"]  "Term: Trim spaces"
    , Z.flag ""  ["trim-begin"]         "Term: Trim spaces beginning of value"
    , Z.flag ""  ["trim-end"]           "Term: Trim spaces end of value"
    , Z.opt  ""  ["seq"] "/TERM"        "Term: Add sequential number term (default /seq)"
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
    | otherwise  = convertPrintCsvFile (convertCsvString p) `mapM_` paraInput p


-- --------------------------------------------  Parameter

-- | Command parameter.
data Para = Para
    { paraBody       :: Int               -- ^ Line number at body starts
    , paraSeq        :: Maybe K.TermName  -- ^ Add sequential number term
    , paraEmacsMode  :: Bool              -- ^ Inlucde emacs mode
    , paraIncludes   :: [String]          -- ^ Inlucde lines
    , paraLicenses   :: [String]          -- ^ License lines
    , paraJudgeType  :: JudgeType         -- ^ Judgement class and term names.
    , paraTrim       :: (Bool, Bool)      -- ^ Trim value
    , paraEmpty      :: Bool              -- ^ Convert empty string
    , paraDecimal    :: Bool              -- ^ Convert decimal number
    , paraInput      :: [FilePath]        -- ^ Input CSV files

    , paraHelp       :: Bool
    , paraVersion    :: Bool
    } deriving (Show, Eq, Ord)

-- | Initialize parameter.
initPara :: Z.Parsed -> IO Para
initPara (Left errs) = error $ unwords errs
initPara (Right (z, args)) =
    do comment <- readTextFiles $ req "include"
       license <- readTextFiles $ req "license"
       return $ Para { paraBody       = body
                     , paraSeq        = K.toTermName <$> opt "seq" "/seq"
                     , paraEmacsMode  = flag "emacs-mode"
                     , paraIncludes   = comment
                     , paraLicenses   = license
                     , paraJudgeType  = numericJudgeType "CSV"
                     , paraTrim       = trim
                     , paraEmpty      = flag "to-empty"
                     , paraDecimal    = flag "to-decimal"
                     , paraInput      = args

                     , paraHelp       = flag "help"
                     , paraVersion    = flag "version" }
    where
      flag  = Z.getFlag    z
      req   = Z.getReq     z
      opt   = Z.getOptLast z

      body = K.fromMaybe 1 $ do
               n <- opt "body" "1"
               K.stringInt n

      trim | flag "trim"       = (True, True)
           | flag "trim-both"  = (True, True)
           | flag "trim-left"  = (True, False)
           | flag "trim-right" = (False, True)
           | otherwise         = (False, False)

readTextFiles :: [FilePath] -> IO [String]
readTextFiles paths =
    do contents <- mapM readFile paths
       return $ concatMap lines contents


-- --------------------------------------------  Judgement type

data JudgeType =
    JudgeType
      { judgeTypeClass  :: K.JudgeClass
      , judgeTypeTerms  :: [K.TermName]
      } deriving (Show, Eq, Ord)

-- | Judgement type of numeric term names.
numericJudgeType :: K.JudgeClass -> JudgeType
numericJudgeType cl = JudgeType cl (K.toTermName <$> K.ints 1)


-- --------------------------------------------  Convert CSV file

-- | Convert and print CSV file to Koshucode.
convertPrintCsvFile :: K.ManyMap String -> FilePath -> IO ()
convertPrintCsvFile f path =
    do content <- readFile path
       K.putLines $ f content

-- | Convert CSV string.
convertCsvString :: Para -> K.ManyMap String
convertCsvString p s =
    case K.plainEncode <$> parseConvertCsv p s of
      body -> appendHeader (paraEmacsMode p)
                           (paraIncludes p)
                           (paraLicenses p)
                           body

appendHeader :: Bool -> [String] -> [String] -> K.Map [String]
appendHeader mode include license body = body' where
    body'     = appendBlock [mode', include', license', body]
    pad s     = "  " ++ s :: String
    mode'     | mode          = ["** -*- koshu -*-"]
              | otherwise     = []
    include'  | null include  = []
              | otherwise     = include
    license'  | null license  = []
              | otherwise     = ["=== license", ""]
                                  ++ map pad license
                                  ++ ["", "=== rel"]

appendBlock :: [[String]] -> [String]
appendBlock [] = []
appendBlock ([] : bs) = appendBlock bs
appendBlock [b]       = b
appendBlock (b : bs)  = b ++ [""] ++ appendBlock bs


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
    judge = csvJudge (paraSeq p) (paraEmpty p, paraDecimal p) (paraJudgeType p)
    csv'  = omitEmptyLines
              $ K.map2 (trimValue $ paraTrim p)
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
csvJudge :: Maybe K.TermName -> (Bool, Bool) -> JudgeType -> Int -> Csv.Record -> K.JudgeC
csvJudge maybeSeq to (JudgeType cl ns) n values
    = case maybeSeq of
        Nothing    -> K.affirm cl ts
        Just name -> K.affirm cl $ (name, K.pInt n) : ts
    where
      ts  = zip ns (termC to <$> values)

-- | Create term content.
termC :: (Bool, Bool) -> String -> K.Content
termC (True, _) "" = K.empty
termC (_, False) c = K.pText c
termC (_, True)  c = case K.decodeDecimal c of
                       Right d -> K.pDec d
                       _       -> K.pText c

