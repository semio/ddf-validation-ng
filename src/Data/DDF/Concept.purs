module Data.DDF.Concept where

import Data.Validation.Semigroup
import Prelude

import Data.Csv (CsvRow(..))
import Data.DDF.CsvFile (Header(..), headersExists)
import Data.DDF.FileInfo (FileInfo(..))
import Data.DDF.Identifier (Identifier)
import Data.DDF.Identifier as Id
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.DDF.Validation.Result as Res
import Data.DDF.Validation.ValidationT (Validation, vError, vWarning)
import Data.Either (Either(..))
import Data.List (List(..), concatMap, elem, elemIndex, length, zip)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NL
import Data.Map (Map, delete, fromFoldable, lookup, pop)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Validation.Semigroup (V, invalid)
import Safe.Coerce (coerce)


-- | Types of concepts
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

parseConceptType :: String -> V Errors ConceptType
parseConceptType x = ado
  cid <- Id.parseId x
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

-- | Each Concept should have an Id and concept type.
-- | Other properties will be a dictionary
data Concept
  = Concept
    { conceptId :: Identifier
    , conceptType :: ConceptType
    , props :: Props
    }

-- | Properties type
type Props
  = Map Identifier String

-- | create concept
concept :: Identifier -> ConceptType -> Props -> Concept
concept conceptId conceptType props = Concept { conceptId, conceptType, props }

instance showConcept :: Show Concept where
  show (Concept x) = show x

getId :: Concept -> String
getId (Concept x) = unwrap $ x.conceptId

-- Below are types for unpure world
type ConceptInput
  = { conceptId :: String, conceptType :: String, props :: Props }

-- | convert a ConceptInput into valid Concept or errors
parseConcept :: ConceptInput -> V Errors Concept
parseConcept { conceptId: cid, conceptType: ct, props: props } =
  let
    missingField = (Str.null cid) || (Str.null ct)
  in
    if missingField then
      invalid [ Error "concept or concept_type field is empty." ]
    else
      let
        conceptId = Id.parseId cid

        conceptType = parseConceptType ct
      in
        concept <$> conceptId <*> conceptType <*> pure props

fromCsvRow :: (NonEmptyList Header) -> CsvRow -> V Errors ConceptInput
fromCsvRow headers (CsvRow (Tuple idx row)) =
  if NL.length headers /= L.length row then
    invalid [ Error $ "Line " <> show idx <> ": Bad csv row" ]
  else
    pure $ (mkConcept <<< rowAsMap) row
  where
  -- FIXME: coerce header to Id might not be wrong
  -- because not all headers are valid Ids. (i.e. is-- headers)
  headersL = NL.toList $ map coerce headers  

  rowAsMap r = fromFoldable (zip headersL r)

  mkConcept m =
    let
      conceptCol = (Id.unsafeCreate "concept")

      conceptTypeCol = (Id.unsafeCreate "concept_type")

      cid = lookup conceptCol m

      cid' = case cid of
        Nothing -> ""
        Just s -> s

      ct = lookup conceptTypeCol m

      ct' = case ct of
        Nothing -> ""
        Just s -> s

      m' = m # (delete conceptCol) <<< (delete conceptTypeCol)
    in
      { conceptId: cid', conceptType: ct', props: m' }

-- | read a concept csv file and return list of unvalidated concept objects
-- fromCsvContent :: CsvContent -> Validation Results (Array Concept)
-- fromCsvContent { headers, rows } = do
-- fromCsvFile :: CsvFile -> Array (V Results Concept)
-- fromCsvFile csvfile = 
--   let 
--     FileInfo (_, _, fn) = getFileInfo csvfile
--     { headers, rows } = getCsvContent csvfile
--     go :: Int -> Array (Array String) -> Array (V Results Concept)
--     go ln [row:rows] = 
--       c : go (ln+1) rows
--     (fromCsvRow headers) row `andThen` validateConcept
--   in
--     go 0 rows
