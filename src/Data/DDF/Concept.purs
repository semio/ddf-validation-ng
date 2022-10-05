module Data.DDF.Concept where

import Data.Validation.Semigroup
import Prelude

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Narr
import Data.Csv (CsvRow(..))
import Data.DDF.CsvFile (Header(..), CsvRec, headersExists)
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
import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.String.NonEmpty.Internal (NonEmptyString(..), toString)
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
  | CustomC Identifier

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

derive instance eqConceptType :: Eq ConceptType

parseConceptType :: String -> V Errors ConceptType
parseConceptType x = ado
  cid <- Id.parseId x
  let
    res = case toString $ unwrap cid of
      "string" -> StringC
      "meaeure" -> MeasureC
      "bollean" -> BooleanC
      "interval" -> IntervalC
      "entity_domain" -> EntityDomainC
      "entity_set" -> EntitySetC
      "role" -> RoleC
      "composite" -> CompositeC
      "time" -> TimeC
      _ -> CustomC cid
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

instance eqConcept :: Eq Concept where
  eq (Concept a) (Concept b) =
    a.conceptId == b.conceptId

getId :: Concept -> NonEmptyString
getId (Concept x) = unwrap $ x.conceptId

-- | Concept Input, which comes from CsvFile.
-- | if CsvFile is valid, then every concept should have conceptId and conceptType.
type ConceptInput
  = { conceptId :: String, conceptType :: String, props :: Props }

hasProp :: String -> Props -> Boolean
hasProp f props = M.member (Id.unsafeCreate f) props

checkMandatoryField :: Concept -> V Errors Concept
checkMandatoryField input@(Concept c) = case c.conceptType of
  EntitySetC ->
    if hasProp "domain" c.props then
      pure input
    else
      invalid [ Error $ "missing domain field for entity_set: " <> (toString $ getId input) ]
  _ -> pure input

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
        (concept <$> conceptId <*> conceptType <*> pure props)
          `andThen`
            checkMandatoryField

-- TODO: complete below function
-- conceptInputFromCsvRec :: CsvRec -> V Errors ConceptInput
-- conceptInputFromCsvRec recMap =
--   { conceptId: cid, conceptType: ct, props: m }
--   where
--     recMap' = M.map
--     cid = case

fromCsvRow :: (NonEmptyArray Header) -> (Array String) -> V Errors ConceptInput
fromCsvRow headers row =
  -- TODO: move the Bad row checking to other function in CsvFile module
  if Narr.length headers /= A.length row then
    invalid [ Error $ "Bad csv row" ]
  else
    pure $ (mkConcept <<< rowAsMap) row
  where
  -- FIXME: coerce header to Id might be wrong
  -- because not all headers are valid Ids. (i.e. is-- headers)
  headersL = Narr.toArray $ map coerce headers

  rowAsMap r = fromFoldable (A.zip headersL r)

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
