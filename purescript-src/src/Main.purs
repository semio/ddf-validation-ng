module Main where

import Prelude

import Data.Array (concat, filter, partition)
import Data.Array as Arr
import Data.Csv (CsvRow(..), RawCsvContent, getLineNo, readCsv)
import Data.Csv as C
import Data.DDF.Concept (Concept(..), fromCsvRow, parseConcept)
import Data.DDF.Concept as Conc
import Data.DDF.CsvFile (CsvContent, CsvFile(..), getCsvContent, validCsvFile)
import Data.DDF.CsvFile as CSV
import Data.DDF.DataSet (DataSet(..))
import Data.DDF.FileInfo (isConceptFile, FileInfo(..))
import Data.DDF.FileInfo as FI
import Data.DDF.FileInfo as FileInfo
import Data.DDF.Validation.Result (Error(..), Errors)
import Data.DDF.Validation.ValidationT (Validation, ValidationT, runValidationT, vError, vWarning)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..), snd)
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
validateCsvFile :: FilePath -> RawCsvContent -> Validation Errors CsvFile
validateCsvFile fp csvcontent = case csv of
  Right csv' -> pure csv'
  Left err -> vError err
  where
  csv = toEither $ FI.validateFileInfo fp `andThen` (\fi -> validCsvFile fi csvcontent)

validateConceptsFile :: CsvFile -> Validation Errors (List Concept)
validateConceptsFile csvfile =
  let
    { headers, rows } = getCsvContent csvfile
  in
    do
      concLst <- for rows $ \r -> 
        case toEither $ fromCsvRow headers r `andThen` parseConcept of
          Right c -> pure $ L.singleton c
          Left errs -> do
            let 
              idx = getLineNo r
            vWarning [ Error $ "line " <> show idx <> " has problem" ]
            vWarning errs
            pure Nil
      pure $ L.concat concLst

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
