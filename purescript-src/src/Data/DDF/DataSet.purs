module Data.DDF.DataSet where

import Prelude

import Data.DDF.Concept (Concept(..), getId)
import Data.DDF.Concept as Conc
import Data.DDF.FileInfo (FileInfo(..))
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.DDF.Validation.Result as Res
import Data.DDF.Validation.ValidationT (Validation, vWarning)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NArr
import Data.Either (Either(..))
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation.Semigroup (V, invalid, isValid, toEither)

type ConceptDict
  = Map String Concept

data DataSet
  = DataSet { concepts :: ConceptDict }

-- | create empty dataset
empty :: DataSet
empty = DataSet { concepts: Map.empty }

conceptsStr :: DataSet -> Set String
conceptsStr (DataSet ds) = Map.keys ds.concepts

-- functions to add stuffs to a dataset
addConcept :: Concept -> DataSet -> V Errors DataSet
addConcept conc (DataSet ds) = case concExisted of
  true -> invalid [ Error $ "concept " <> cid <> " existed in dataset" ]
  false -> pure newds
    where
    newds = DataSet $ ds { concepts = newconcepts }

    newconcepts = Map.insert cid conc ds.concepts
  where
  cid = Conc.getId conc

  concExisted = Map.member cid ds.concepts

-- below are per-file checkings

-- | add concepts
-- validateNewConcepts :: (Array Concept) -> DataSet -> Validation (Array Concept)
-- validateNewConcepts concepts dataset =
--   case NArr.fromArray concepts of
--     Nothing -> pure []
--     Just concs -> 
--       let 
--         cids = map getId concs
--       in
--         do 
--           case checkDups cids of
--             Left e -> vWarning e
--             Right _ -> void unit
