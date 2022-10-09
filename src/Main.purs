module Main where

import Prelude

import Control.Monad.State.Trans (get)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, filter, foldM, partition)
import Data.Array as A
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray, groupAllBy, groupBy)
import Data.Csv (CsvRow(..), RawCsvContent, getLineNo, getRow, readCsv)
import Data.Csv as C
import Data.DDF.Concept (Concept(..), fromCsvRow, parseConcept)
import Data.DDF.Concept as Conc
import Data.DDF.CsvFile (CsvContent, CsvFile(..), Header(..), getCsvContent, validCsvFile)
import Data.DDF.CsvFile as CSV
import Data.DDF.DataSet (DataSet(..))
import Data.DDF.DataSet as DataSet
import Data.DDF.FileInfo (FileInfo(..), getCollectionFiles, isConceptFile)
import Data.DDF.FileInfo as FI
import Data.DDF.Validation.Result (Error(..), Errors, Messages, hasError, messageFromError, setError, setFile, setLineNo, setSuggestions)
import Data.DDF.Validation.ValidationT (Validation, ValidationT(..), runValidationT, vError, vWarning)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.JSON.DataPackage (datapackageExists)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation.Semigroup (V, andThen, invalid, isValid, toEither)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log, logShow)
import Node.Path (FilePath)
import Node.Process (argv)
import Utils (arrayOfLeft, arrayOfRight, getFiles)

parseFileInfos :: Array FilePath -> Validation Messages (Array FileInfo)
parseFileInfos fps = do
  fis <-
    for fps
      $ \fp -> do
          let
            res = toEither $ FI.validateFileInfo fp
          case res of
            Right x -> pure [ x ]
            Left errs -> do
              let
                msgs = map (setFile fp <<< messageFromError) errs
              vWarning msgs
              pure []
  pure $ A.concat fis

parseCsvFiles :: Array (Tuple FileInfo RawCsvContent) -> Validation Messages (Array CsvFile)
parseCsvFiles inputs = do
  fs <-
    for inputs
      $ \(Tuple f r) -> do
          let
            fp = FI.filepath f
          case toEither $ CSV.validCsvFile f r of
            Right x -> pure [ x ]
            Left errs -> do
              let
                msgs = map (setFile fp <<< messageFromError) errs
              vWarning msgs
              pure []
  pure $ A.concat fs

appendConceptCsv :: CsvFile -> DataSet -> Validation Messages DataSet
appendConceptCsv csv dataset = do
  let
    { headers, rows } = CSV.getCsvContent csv

    fp = FI.filepath $ CSV.getFileInfo csv

    run hs ds (CsvRow (Tuple idx row)) = do
      let
        concept = Conc.fromCsvRow hs row `andThen` Conc.parseConcept
      case toEither $ concept of
        Left errs -> do
          let
            msgs = map (setError <<< setFile fp <<< setLineNo (idx + 1) <<< messageFromError) errs
          vWarning msgs
          pure ds
        Right conc -> do
          case toEither $ DataSet.addConcept conc ds of
            Left errs' -> do
              let
                msgs = map (setError <<< setFile fp <<< setLineNo (idx + 1) <<< messageFromError) errs'
              vWarning msgs
              pure ds
            Right newds -> pure newds
  foldM (\d r -> run headers d r) dataset rows

runMain :: FilePath -> Effect Unit
runMain fp = do
  -- check if datapackage exists
  datapackageFile <- datapackageExists fp
  case toEither datapackageFile of
    Left errs -> do
      let
        msgs = map (setFile fp <<< messageFromError) errs
      log $ joinWith "\n" $ map show msgs
      log "❌ Dataset is invalid"
    Right _ -> do
      -- load all files
      files <- getFiles fp [ ".git", "etl", "lang", "assets" ]
      (Tuple msgs ds) <-
        runValidationT
          $ do
              fileInfos <- parseFileInfos files
              when (A.length fileInfos == 0)
                $ vError [ messageFromError $ Error "No csv files in this folder. Please begin with a ddf--concepts.csv file." ]
              -- validate Concept files
              let
                conceptFiles = getCollectionFiles "concepts" fileInfos
              when (A.length conceptFiles == 0)
                $ vError [ messageFromError $ Error "No concepts csv files in this folder. Dataset must at least have a ddf--concepts.csv file." ]
              conceptCsvContents <- lift $ C.readCsvs $ map FI.filepath conceptFiles
              let
                conceptInputs = A.zip conceptFiles conceptCsvContents
              conceptCsvFiles <- parseCsvFiles conceptInputs
              -- create a dataset and append all Concepts parsed
              ds <- foldM (\d f -> appendConceptCsv f d) DataSet.empty conceptCsvFiles
              -- TODO: after loading all concepts, need to check if all properties keys are valid Concepts
              -- validate Entity files
              let
                entityFiles = getCollectionFiles "entities" fileInfos
              when (A.length entityFiles > 0)
                $ do
                    entityCsvContents <- lift $ C.readCsvs $ map FI.filepath entityFiles
                    let
                      entityInputs = A.zip entityFiles entityCsvContents
                    void $ parseCsvFiles entityInputs
              -- next steps...
              -- finally if everything goes well, return the dataset
              pure ds
      -- show all the error messages
      log $ joinWith "\n" $ map show msgs
      case ds of
        Just _ -> 
          if hasError msgs then
            log "❌ Dataset is invalid"
          else
            log "✅ Dataset is valid"
        Nothing -> log "❌ Dataset is invalid"
      pure unit

-- logShow conceptFiles
main :: Effect Unit
main = do
  -- get path
  path <- argv
  case path A.!! 2 of
    Nothing -> runMain "./"
    Just fp -> runMain fp
