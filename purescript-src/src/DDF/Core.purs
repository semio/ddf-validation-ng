module DDF where

import DDF.Types.Concept as Concept
import DDF.Types.FileInfo (FileInfo(..))
import DDF.Types.Identifier as Identifier
import Data.Map (Map)

type ConceptDict
  = Map String Concept.Concept

data DDF
  = DDF { concepts :: ConceptDict }

-- input =
--   [ { concept: "testing"
--     , concept_type: "entity_domain"
--     }
--   , { concept: "name"
--     , concept_type: "string"
--     }
--   ]

-- fromFiles :: Array FileInfo -> 


-- Read all csv files
-- build metadata for each file
-- collection
-- primary key columns
-- data key columns
-- constrains
-- Rule 1: Collection type should be in ...
-- Rule 2: 

-- Read concepts file
-- Rule 1: it must has concept and concept_type field


-- Rule 2: it must not have duplicated concept


-- Rule 3: 