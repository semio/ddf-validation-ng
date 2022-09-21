module Main where

import Prelude

import Csv (RawCsvContent, readCsv)
import Csv as C
import DDF.Types.Concept (Concept(..), fromCsvContent, validateConcept)
import DDF.Types.CsvFile (CsvContent, CsvFile(..), getCsvContent, validCsvFile)
import DDF.Types.CsvFile as CSV
import DDF.Types.FileInfo (isConceptFile, FileInfo(..))
import DDF.Types.FileInfo as FI
import DDF.Types.FileInfo as FileInfo
import DDF.Validation.Types.Result (Errors, getValue)
import DDF.Validation.Types.ValidationT (Validation, ValidationT, runValidationT, vError, vWarning)
import Data.Array (concat, filter, partition)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, andThen, invalid, isValid, toEither)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log, logShow)
import Node.Path (FilePath)
import Utils (arrayOfLeft, arrayOfRight, getFiles)


-- step1: read all files from dataset folder
-- validateFiles :: FilePath -> ValidationT Results Effect (Array FileInfo)
-- validateFiles

-- step2: validate if it's good csv file
validateCsvFile :: FilePath -> RawCsvContent -> Validation CsvFile
validateCsvFile fp csvcontent =
  case fileinfo of
    Right fi ->
      validCsvFile fi csvcontent
    Left err -> vError err
  where 
    fileinfo = toEither $ FI.validateFileInfo fp


validateConceptsFile :: CsvFile -> Validation (Array Concept)
validateConceptsFile csvfile = 
  let 
    conceptinput = fromCsvContent $ getCsvContent csvfile
    res = map (toEither <<< validateConcept) conceptinput
  in
    do 
    concArr <- for res $ \x ->
      case x of
        Right conc -> pure [ conc ]
        Left err -> do
          vWarning err
          pure []
    pure $ concat concArr


main2 :: Effect Unit
main2 = do
  let
    path = "/home/semio/src/work/gapminder/libs/ddf-validation-ng/purescript-src/test/datasets/test1/ddf--concepts.csv"
  lst <- readCsv path
  let
    csvContent = C.create lst

  res <- runValidationT $ do 
    csvfile <- validateCsvFile path csvContent
    validateConceptsFile csvfile
    -- let
    --   vcsvcontent = getCsvContent vcsv
    -- pure $ fromCsvContent vcsvcontent
  logShow res


main :: Effect Unit
main = do
  -- step 1: load all files
  files <- getFiles "/home/semio/src/work/gapminder/libs/ddf-validation-ng/purescript-src/test/datasets/test1"
  -- log "gathering dataset files..."
  -- let
  -- allFileInfo = map FI.create files
  -- -- file info
  -- { no: errs, yes: valids } = partition (isValid) allFileInfo
  -- goodFiles = concat $ map (arrayOfRight <<< toEither) valids
  -- errs = concat $ map (arrayOfLeft <<< toEither) valids
  -- -- file with correct primary keys
  -- -- TODO: maybe bad files should be also reported.
  -- concepts = filter isConceptFile goodFiles
  -- log "gathering dataset concepts..."
  logShow files