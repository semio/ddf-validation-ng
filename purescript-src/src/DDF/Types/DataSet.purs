module DDF.Types.DataSet where

import Prelude

import DDF.Types.Concept (Concept(..), getId)
import DDF.Types.Concept as Conc
import DDF.Types.FileInfo (FileInfo(..))
import DDF.Validation.Types.Result (Results)
import DDF.Validation.Types.Result as Res
import DDF.Validation.Types.ValidationT (Validation, vWarning)
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

-- below are per-file checkings

-- | create a counter
counter :: forall a. Ord a => Eq a => NonEmptyArray a -> NonEmptyArray (Tuple a Int)
counter xs = map (\x -> (Tuple (NArr.head x) (NArr.length x))) <<< NArr.group <<< NArr.sort $ xs

-- | check if there are duplicated entries in a list
checkDups :: forall a. Show a => Eq a => Ord a => NonEmptyArray a -> V Results (NonEmptyArray a)
checkDups xs =
  let
    ns = counter xs

    hasDups = NArr.filter (\x -> (snd x) > 1) ns
  in
    case Arr.head hasDups of
      Nothing -> pure xs
      Just _ -> do
        let
          allDups = map fst hasDups

          msg = "duplicated entry: " <> show allDups
        invalid [ Res.create "" msg "warning" ]

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
