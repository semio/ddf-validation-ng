module Data.DDF.FileInfo where

import Prelude
import Control.Alt ((<|>))
import Data.DDF.Identifier (identifier)
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripSuffix)
import Data.Validation.Semigroup (V, invalid, toEither)
import Node.Path (FilePath, basename)
import StringParser (Parser, choice, eof, runParser, string, try)

type Ent
  = { domain :: String, set :: Maybe String }

type DP
  = { indicator :: String, pkeys :: Array String, constrains :: Array (Maybe String) }

data CollectionInfo
  = Concepts
  | Entities Ent
  | DataPoints DP
  | Other String

-- data FileInfo
--   = FileInfo
--     { path :: String
--     , collection :: CollectionInfo
--     , name :: String
--     }

-- | file info are information contains in file name.
data FileInfo
  = FileInfo FilePath CollectionInfo String

instance showCollection :: Show CollectionInfo where
  show Concepts = "concepts"
  show (Entities e) = case e.set of
    Nothing -> "entity_domain: " <> show e.domain
    Just s -> "entity_domain: " <> show e.domain <> "; entnty_set: " <> s
  show (DataPoints d) = "datapoints: " <> show d.indicator
  show (Other x) = "custom collection: " <> show x

instance showFileInfo :: Show FileInfo where
  show (FileInfo fp ci _) = "file: " <> fp <> "; collection: " <> show ci

isConceptFile :: FileInfo -> Boolean
isConceptFile (FileInfo _ collection _) = case collection of
  Concepts -> true
  _ -> false

-- 
-- Below are parsers for ddf file names
-- 
ddfFileBegin :: Parser Unit
ddfFileBegin = void $ string "ddf--"

-- ddfFileEnd :: Parser Unit
-- ddfFileEnd = string "" *> eof
-- Parse concept file name
conceptFile :: Parser CollectionInfo
conceptFile = do
  let
    allInOne = pure $ string "ddf--concepts" <* eof

    sepInTwo = do
      p1 <- string "ddf--concepts--"
      p2 <- string "discrete" <|> string "continuous"
      eof
      pure $ p1 <> p2
  choice [ allInOne sepInTwo ] *> pure Concepts

-- parse Entity file name
e1 :: Parser CollectionInfo
e1 = do
  ddfFileBegin
  void $ string "entities--"
  domain <- identifier
  eof
  pure $ Entities { domain: domain, set: Nothing }

e2 :: Parser CollectionInfo
e2 = do
  ddfFileBegin
  void $ string "entities--"
  domain <- identifier
  void $ string "--"
  eset <- identifier
  eof
  pure $ Entities { domain: domain, set: Just eset }

entityFile ∷ Parser CollectionInfo
entityFile = choice [ try e2, try e1 ]

-- TODO: add datapoint file parsers
--
getName :: String -> Maybe String
getName = stripSuffix (Pattern ".csv")

validateFileInfo :: FilePath -> V Errors FileInfo
validateFileInfo fp = case getName $ basename fp of
  Nothing -> invalid [ Error $ fp <> "is not a csv file" ]
  Just fn ->
    let
      fileParser = conceptFile <|> entityFile
    in
      case runParser fileParser fn of
        Right ci -> pure $ FileInfo fp ci fn
        Left err -> invalid [ Error $ fp <> "is not correct ddf file: " <> err.error ]

fromFilePath :: FilePath -> Either Errors FileInfo
fromFilePath = toEither <<< validateFileInfo
