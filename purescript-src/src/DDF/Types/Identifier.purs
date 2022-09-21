module DDF.Types.Identifier where

import Data.Validation.Semigroup
import Prelude
import StringParser

import Control.Alt ((<|>))
import DDF.Validation.Types.Result (Errors, Ok(..), Result, Results, Warnings)
import DDF.Validation.Types.Result as Res
import Data.Array (foldr, fromFoldable)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
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

-- | parse identifier strings. First char must be alphanum.
identifier :: Parser String
identifier = do
  firstchar <- alphaNumLower
  otherchars <- many alphaNumAnd_
  pure $ stringFromChars (firstchar : otherchars)


-- | check if the whole string is an identifer
identifier' :: Parser String
identifier' = identifier <* eof


validateId :: String -> V Errors Identifier
validateId x =
  case runParser identifier' x of
      Right str -> pure $ Id str
      Left e -> invalid [err]
        where
          pos = show $ e.pos
          msg = e.error <> ", pos: " <> pos
          err = Res.create x msg "error"


-- validIds :: Array String -> V Issues (Array Identifier)
-- validIds xs = foldr (<>) vs (pure [])
--   where
--     vs = map validId xs

create :: String -> Either Results Identifier
create x = toEither $ validateId x



