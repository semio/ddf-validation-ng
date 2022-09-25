module Data.DDF.Entity where

import Prelude

import Data.DDF.Identifier (Identifier)
import Data.Map (Map)

data Entity
  = Entity
    { entityId :: Identifier
    , entityDomain :: Identifier
    , props :: Props
    }

instance eqEntity :: Eq Entity where
    eq (Entity a) (Entity b) = 
        (a.entityId == b.entityId) && (a.entityDomain == b.entityDomain)

instance showEntity :: Show Entity where
    show (Entity x) = show x

-- | Properties type
type Props
  = Map Identifier String



