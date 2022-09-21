module DDF.Types.CsvFile where

import Prelude

import Csv (RawCsvContent)
import DDF.Types.FileInfo (FileInfo(..), CollectionInfo(..))
import DDF.Types.FileInfo as FileInfo
import DDF.Types.Identifier as Id
import DDF.Validation.Types.Result (Results, Errors)
import DDF.Validation.Types.Result as Res
import DDF.Validation.Types.ValidationT (Validation, vError, vWarning)
import Data.Array (concat, concatMap, difference, elem, foldr, length, nub)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V, andThen, invalid, toEither)
import Utils (arrayOfLeft)

-- | CsvContent is the data read from a csv file.
type CsvContent
  = { headers :: Array String
    , rows :: Array (Array String)
    }

-- | csv file contains file name info and file content
data CsvFile
  = CsvFile
    { fileInfo :: FileInfo
    , csvContent :: CsvContent
    }

instance showCsvFile :: Show CsvFile where
  show (CsvFile x) = show x

mkCsvContent :: Array String -> Array (Array String) -> CsvContent
mkCsvContent headers rows = { headers: headers, rows: rows }

mkCsvFile :: FileInfo -> CsvContent -> CsvFile
mkCsvFile fi csv = CsvFile { fileInfo: fi, csvContent: csv }

getCsvContent :: CsvFile -> CsvContent
getCsvContent (CsvFile { fileInfo, csvContent }) = csvContent

-- below are validations
--
hasCols :: Array String -> CsvContent -> V Errors CsvContent
hasCols expected csvcontent =
  if res then
    pure csvcontent
  else
    invalid [ Res.create ("expected columns:" <> show expected) "error" "" ]
  where
  res = foldr (\x acc -> x `elem` csvcontent.headers && acc) true expected

-- | check if csv file has headers
validCsvContent :: RawCsvContent -> V Errors CsvContent
validCsvContent input = case input.headers of
  Nothing -> invalid [ Res.create "" "no headers" "error" ]
  Just hs -> case input.rows of
    Nothing -> pure $ { headers: hs, rows: [] }
    Just rs -> pure $ { headers: hs, rows: rs }

-- | check all columns are valid identifiers
colsAreValidIds :: CsvContent -> V Errors CsvContent
colsAreValidIds input = 
  if length errors > 0 then
    invalid errors
  else
    pure input
  where
    res = map Id.create input.headers
    errors = concat $ concatMap arrayOfLeft res 

-- | check if csv file has duplicated headers
noDupCols :: CsvContent -> Validation CsvContent
noDupCols input =
  if nub input.headers == input.headers then
    pure input
  else
    let
      dups = difference (nub input.headers) input.headers
    in
      do
        vWarning [ Res.create "" ("duplicated headers: " <> show dups <> ", only first one will be use") "error" ]
        pure input -- Maybe remove dups

-- | check if file info matched with csv file headers
validCsvFile :: FileInfo -> RawCsvContent -> Validation CsvFile
validCsvFile f@(FileInfo _ collection _) csvcontent = case collection of
  Concepts -> do
    let
      vc = validCsvContent csvcontent `andThen` (hasCols [ "concept", "concept_type" ]) `andThen` colsAreValidIds
    case toEither vc of
      Right vc' -> do
        void $ noDupCols vc'
        pure $ mkCsvFile f vc'
      Left err -> vError err
  otherwise -> vError [ Res.create "" "not impl." "error" ]
