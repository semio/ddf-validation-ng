-- | Validate if it's a valid ddf csv file
module Data.DDF.CsvFile where

import Prelude
import StringParser

import Control.Alt ((<|>))
import Data.Csv (CsvRow, RawCsvContent)
import Data.DDF.FileInfo (FileInfo(..), CollectionInfo(..))
import Data.DDF.FileInfo as FileInfo
import Data.DDF.Identifier (Identifier)
import Data.DDF.Identifier as Id
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.DDF.Validation.ValidationT (Validation, vError, vWarning)
import Data.Either (Either(..), fromLeft, fromRight, isLeft)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as L
import Data.List.NonEmpty (NonEmptyList, elem, fold1, nub)
import Data.List.NonEmpty as NL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.String.Utils (startsWith)
import Data.Traversable (class Foldable, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation.Semigroup (V, andThen, invalid, isValid, toEither)
import StringParser (Parser, choice, eof, runParser, string)

newtype Header
  = Header String

derive instance newtypeHeader :: Newtype Header _

derive instance genericHeader :: Generic Header _

derive instance eqHeader :: Eq Header

derive instance ordHeader :: Ord Header

instance showHeader :: Show Header where
  show = genericShow

-- FIXME: replace this with unwrap
headerVal :: Header -> String
headerVal (Header x) = x

is_header :: Parser String
is_header = do
  begin <- string "is--"
  val <- Id.identifier
  pure $ begin <> val

header :: Parser String
header = do
  h <- is_header <|> Id.identifier
  void $ eof
  pure h

parseHeader :: String -> V Errors Header
parseHeader x = case runParser header x of
  Right str -> pure $ Header str
  Left e -> invalid [ err ]
    where
    pos = show $ e.pos

    msg = "invalid header: " <> x <> ", " <> e.error <> "at pos " <> pos

    err = Error msg

createHeader :: String -> Either Errors Header
createHeader x = toEither $ parseHeader x

unsafeCreateHeader :: String -> Header
unsafeCreateHeader = Header

-- | CsvContent is the data read from a csv file.
type CsvContent
  = { headers :: NonEmptyList Header
    , rows :: List CsvRow
    }

-- | csv file combines file name info and file content
data CsvFile
  = CsvFile
    { fileInfo :: FileInfo
    , csvContent :: CsvContent
    }

instance showCsvFile :: Show CsvFile where
  show (CsvFile x) = show x

mkCsvContent :: NonEmptyList Header -> List CsvRow -> CsvContent
mkCsvContent headers rows = { headers: headers, rows: rows }

mkCsvFile :: FileInfo -> CsvContent -> CsvFile
mkCsvFile fi csv = CsvFile { fileInfo: fi, csvContent: csv }

getCsvContent :: CsvFile -> CsvContent
getCsvContent (CsvFile { csvContent }) = csvContent

getFileInfo :: CsvFile -> FileInfo
getFileInfo (CsvFile { fileInfo }) = fileInfo

-- below are intermediate types and validations
--
type NonEmptyRawCsvContent
  = { headers :: NonEmptyList String
    , rows :: List CsvRow
    }

-- | function that checks if first list is subset of second list
-- use this to check if required columns are existed.
hasCols :: forall t a. Foldable t => Ord a => Eq a => t a -> t a -> Boolean
hasCols expected actual =
  let
    expectedSet = S.fromFoldable expected

    actualSet = S.fromFoldable actual
  in
    S.subset expectedSet actualSet

-- | check if csv file has headers
notEmptyCsv :: RawCsvContent -> V Errors NonEmptyRawCsvContent
notEmptyCsv input = case join $ NL.fromList <$> input.headers of
  Nothing -> invalid [ Error "no headers" ]
  Just hs -> case input.rows of
    Nothing -> pure $ { headers: hs, rows: Nil }
    Just rs -> pure $ { headers: hs, rows: rs }

-- | check all columns are valid identifiers
colsAreValidIds :: NonEmptyRawCsvContent -> V Errors CsvContent
colsAreValidIds input =
  let
    res = sequence $ map parseHeader input.headers
  in
    case toEither res of
      Right hs ->
        let
          headerValues = map headerVal hs

          is_headers = NL.filter (startsWith "is--") headerValues
        in
          case is_headers of
            Nil -> pure $ input { headers = hs }
            xs -> invalid [ Error $ "these headers are not valid Ids: " <> show xs ]
      Left errs -> invalid errs

-- | check all columns are valid headers (including is-- headers)
colsAreValidHeaders :: NonEmptyRawCsvContent -> V Errors CsvContent
colsAreValidHeaders input =
  let
    res = sequence $ map parseHeader input.headers
  in
    case toEither res of
      Right hs -> pure $ input { headers = hs }
      Left errs -> invalid errs

-- | check required concept headers
headersExists :: Array String -> NonEmptyRawCsvContent -> V Errors NonEmptyRawCsvContent
headersExists expected csvcontent =
  let
    requiredFields = L.fromFoldable expected

    actual = L.fromFoldable csvcontent.headers
  in
    if hasCols requiredFields actual then
      pure csvcontent
    else
      invalid [ Error $ "concept file MUST have following field: " <> show expected ]

-- | check if csv file has duplicated headers.
noDupCols :: CsvContent -> Validation Errors CsvContent
noDupCols input =
  if nub input.headers == input.headers then
    pure input
  else
    let
      counter = map (\x -> (Tuple (NL.head x) (NL.length x))) <<< NL.group <<< NL.sort $ input.headers

      dups = NL.filter (\x -> (snd x) > 1) counter
    in
      do
        vWarning [ Error $ "duplicated headers: " <> show dups <> ", only first one will be use" ]
        pure input -- Maybe remove dups

-- | check if file info matched with csv file headers
validCsvFile :: FileInfo -> RawCsvContent -> V Errors CsvFile
validCsvFile f@(FileInfo _ collection _) csvcontent = case collection of
  Concepts ->
    let
      required = [ "concept", "concept_type" ]

      vc =
        notEmptyCsv csvcontent
          `andThen`
            headersExists required
          `andThen`
            colsAreValidIds
    in
      mkCsvFile <$> pure f <*> vc
  otherwise -> invalid [ Error "not impl." ]

-- TODO: the complete function that add warning about duplicated columns
