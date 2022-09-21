module DDF.Types.Concept where

import Data.Validation.Semigroup
import Prelude

import Control.Monad.Except (mapExceptT)
import DDF.Types.CsvFile (CsvContent)
import DDF.Types.Identifier as Id
import DDF.Validation.Types.Result (Result, Results)
import DDF.Validation.Types.Result as Res
import DDF.Validation.Types.ValidationT (Validation, vError, vWarning)
import Data.Array (concatMap, elem, elemIndex, zip)
import Data.Either (Either(..))
import Data.Map (Map, delete, fromFoldable, lookup, pop)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V, invalid)

type Props
  = Map String String

data ConceptType
  = StringC
  | MeasureC
  | BooleanC
  | IntervalC
  | EntityDomainC
  | EntitySetC
  | RoleC
  | CompositeC
  | TimeC
  | CustomC String -- This can be any string

instance showConceptType :: Show ConceptType where
  show StringC = "string"
  show MeasureC = "measure"
  show BooleanC = "boolean"
  show IntervalC = "interval"
  show EntityDomainC = "entity_domain"
  show EntitySetC = "enitty_set"
  show RoleC = "role"
  show CompositeC = "composite"
  show TimeC = "time"
  show (CustomC x) = show x

createConceptType :: String -> V Results ConceptType
createConceptType x = ado
  cid <- Id.validateId x
  let
    res = case unwrap cid of
      "string" -> StringC
      "meaeure" -> MeasureC
      "bollean" -> BooleanC
      "interval" -> IntervalC
      "entity_domain" -> EntityDomainC
      "entity_set" -> EntitySetC
      "role" -> RoleC
      "composite" -> CompositeC
      "time" -> TimeC
      ct -> CustomC ct
  in res

data Concept
  = Concept
    { conceptId :: Id.Identifier
    , conceptType :: ConceptType
    , props :: Props
    }

concept :: Id.Identifier -> ConceptType -> Props -> Concept
concept conceptId conceptType props = Concept { conceptId, conceptType, props }

instance showConcept :: Show Concept where
  show (Concept x) = show x

type ConceptInput
  = { conceptId :: String, conceptType :: String, props :: Props }

-- | read a concept csv file and return list of unvalidated concept objects
fromCsvContent :: CsvContent -> Array ConceptInput
fromCsvContent { headers, rows } =
  let
    rowAsMap r = fromFoldable (zip headers r)

    mkConcept m =
      let
        cid = lookup "concept" m

        cid' = case cid of
          Nothing -> ""
          Just s -> s

        ct = lookup "concept_type" m

        ct' = case ct of
          Nothing -> ""
          Just s -> s

        m' = m # (delete "concept") <<< (delete "concept_type")
      in
        { conceptId: cid', conceptType: ct', props: m' }
  in
    map (mkConcept <<< rowAsMap) rows

getId :: Concept -> String
getId (Concept x) = unwrap $ x.conceptId

-- | convert a list of ConceptInput to list of Concept
validateConcept :: ConceptInput -> V Results Concept
validateConcept { conceptId: cid, conceptType: ct, props: props } =
  let
    missingField = (Str.null cid) || (Str.null ct)
  in
    if missingField then
      invalid [ Res.create "" "missing concept concept_type field" "error" ]
    else
      let
        conceptId = Id.validateId cid

        conceptType = createConceptType ct
      in
        concept <$> conceptId <*> conceptType <*> pure props
