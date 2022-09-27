module Data.DDF.Entity where

import Prelude

import Data.DDF.Identifier (Identifier)
import Data.DDF.Validation.Result (Errors)
import Data.List (List(..))
import Data.List as L
import Data.Map (Map)
import Data.Validation.Semigroup (V)

data Entity
  = Entity
    { entityId :: Identifier
    , entityDomain :: Identifier
    , entitySets :: List Identifier
    , props :: Props
    }

instance eqEntity :: Eq Entity where
    eq (Entity a) (Entity b) = -- FIXME: coerce header to Id might not be wrong
        (a.entityId == b.entityId) && (a.entityDomain == b.entityDomain)

instance showEntity :: Show Entity where
    show (Entity x) = show x

-- | Properties type
type Props
  = Map Identifier String


-- below are validations
type EntityInput = 
    { entityId :: String, entityDomain :: String, props :: Map String String }

-- parseEntity :: EntityInput -> V Errors Entity
-- parseEntity = ?x


