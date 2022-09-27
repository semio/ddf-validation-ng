module Data.Csv where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.DDF.Validation.Result as Res
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.List (List(..), (:), length, range, zip, tail, head)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V(..), invalid)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

newtype CsvRow = 
  CsvRow (Tuple Int (List String))

instance showCsvRow :: Show CsvRow where
  show (CsvRow (Tuple i x)) = 
    show rec 
    where 
      rec = { line: i, record: x }

derive instance newtypeCsvRow :: Newtype CsvRow _

foreign import readCsvImpl :: Fn1 FilePath (List (List String))

-- TODO: Maybe Change to Aff
readCsv :: FilePath -> Effect (List (List String))
readCsv x =
  pure $ runFn1 readCsvImpl x

getRow :: CsvRow -> (List String)
getRow (CsvRow tpl) = snd tpl

getLineNo :: CsvRow -> Int
getLineNo (CsvRow tpl) = fst tpl

type RawCsvContent
  = { headers :: Maybe (List String)
    , rows :: Maybe (List CsvRow)
    }

toCsvRow :: List (List String) -> List CsvRow
toCsvRow Nil = Nil
toCsvRow xs = 
  let
    idxs = range 1 (length xs)

    tuples = zip idxs xs

    mkRow tpls = CsvRow tpls
  in 
    map mkRow tuples

create :: (List (List String)) -> RawCsvContent
create recs = { headers: headers, rows: rows }
  where
  headers = head recs

  rows = toCsvRow <$> tail recs

-- TODO: add column view. i.e. convert List CsvRow to List CsvColumn