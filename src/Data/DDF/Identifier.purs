module Data.DDF.Identifier where

import Data.Validation.Semigroup
import Prelude
import StringParser

import Control.Alt ((<|>))
import Data.Array (foldr, fromFoldable)
import Data.DDF.Validation.Result (Errors, Error(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.List.NonEmpty as NL
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)

stringFromChars :: List Char -> String
stringFromChars = fromCharArray <<< fromFoldable

-- Definition of identifiers. They are strings
-- But with a limitation: ...
newtype Identifier = Id String

derive instance newtypeId :: Newtype Identifier _
derive instance genericId :: Generic Identifier _
derive instance eqId :: Eq Identifier
derive instance ordId :: Ord Identifier

instance showId :: Show Identifier where
  show = genericShow

-- FIXME: replace this with unwrap
value :: Identifier -> String
value (Id x) = x

-- | parse lower case alphanum strings
alphaNumLower :: Parser Char
alphaNumLower = lowerCaseChar <|> anyDigit
  <?> "expect lowercase alphanumeric value"

-- | parse lower case alphanum strings also allow underscores inside
alphaNumAnd_ :: Parser Char
alphaNumAnd_ = alphaNumLower <|> char '_'
  <?> "expect lowercase alphanumeric and underscore _"

-- | parse identifier strings.
identifier :: Parser String
identifier = do
  chars <- many1 alphaNumAnd_
  pure $ stringFromChars $ NL.toList chars


-- | check if the whole string is an identifer
identifier' :: Parser String
identifier' = identifier <* eof


-- | parse an id, return the applicative V
parseId :: String -> V Errors Identifier
parseId x =
  case runParser identifier' x of
      Right str -> pure $ Id str
      Left e -> invalid [err]
        where
          pos = show $ e.pos
          msg = "invalid id: " <> x <> ", " <> e.error <> "at pos " <> pos
          err = Error msg

-- | parse an id, return Either err id
create :: String -> Either Errors Identifier
create x = toEither $ parseId x

-- | unsafe create an id, because we won't check the string.
-- | only use this when you know what you are doning
unsafeCreate :: String -> Identifier
unsafeCreate = Id

