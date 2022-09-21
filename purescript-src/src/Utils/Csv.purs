module Csv where

import Prelude

import Control.Promise (Promise, toAffE)
import DDF.Validation.Types.Result (Errors, Warnings)
import DDF.Validation.Types.Result as Res
import Data.Array (elem, foldr, head, tail)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple)
import Data.Validation.Semigroup (V(..), invalid)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

foreign import readCsvImpl :: Fn1 FilePath (Array (Array String))

readCsv :: FilePath -> Effect (Array (Array String))
readCsv x = do
  content <- readTextFile UTF8 x
  pure $ runFn1 readCsvImpl content


type RawCsvContent
  = { headers :: Maybe (Array String)
    , rows :: Maybe (Array (Array String))
    }

create :: (Array (Array String)) -> RawCsvContent
create recs = { headers: headers, rows: rows }
  where
  headers = head recs

  rows = tail recs