module Data.DDF.DataSet where

import Prelude

import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NArr
import Data.DDF.Concept (Concept(..), getId)
import Data.DDF.Concept as Conc
import Data.DDF.FileInfo (FileInfo(..))
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.DDF.Validation.Result as Res
import Data.DDF.Validation.ValidationT (Validation, vWarning)
import Data.Either (Either(..))
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty.Internal (NonEmptyString(..), toString)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation.Semigroup (V, invalid, isValid, toEither)

type ConceptDict
  = Map NonEmptyString Concept

data DataSet
  = DataSet { concepts :: ConceptDict }

-- | create empty dataset
empty :: DataSet
empty = DataSet { concepts: Map.empty }

conceptsStr :: DataSet -> Set NonEmptyString
conceptsStr (DataSet ds) = Map.keys ds.concepts

getConcepts :: DataSet -> ConceptDict
getConcepts (DataSet { concepts }) = concepts

hasConcept :: DataSet -> Concept -> Boolean
hasConcept (DataSet ds) conc =
  Map.member cid ds.concepts
  where
    cid = Conc.getId conc

-- functions to add stuffs to a dataset

-- | add concept to Dataset, will fail when concept exists.
addConcept :: Concept -> DataSet -> V Errors DataSet
addConcept conc (DataSet ds) = case concExisted of
  true -> invalid [ Error $ "concept " <> toString cid <> " existed in dataset" ]
  false -> pure newds
    where
    newds = DataSet $ ds { concepts = newconcepts }

    newconcepts = Map.insert cid conc ds.concepts
  where
  cid = Conc.getId conc

  concExisted = Map.member cid ds.concepts

-- | add entity to Dataset, will fail when same entity exists
