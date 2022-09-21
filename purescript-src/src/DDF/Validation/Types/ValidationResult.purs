module DDF.Validation.Types.Result where

import Prelude
import Data.Either (Either(..))
import Data.Validation.Semigroup (V, isValid)

type Error
  = String

type Value
  = String

type ErrorType
  = String

-- | ValidationError Type
data Result
  -- = Result Value Error ErrorType
  = Result
    { value :: Value
    , error :: Error
    , errorType :: ErrorType
    }

type Results
  = Array Result

type Errors
  = Results

type Warnings
  = Results

instance showResult :: Show Result where
  -- show (Result v e t) = "type: " <> e <> "; msg: " <> e <> "; value: " <> v
  show (Result x) = show x

create :: Value -> Error -> ErrorType -> Result
create value error errorType = Result { value, error, errorType }

-- Good result type
data Ok a
  = Good a
  | HasWarning a Warnings

getValue :: forall a. Ok a -> a
getValue (Good x) = x

getValue (HasWarning x _) = x

instance semigroupOk :: Semigroup (Ok a) where
  append (Good _) (Good y) = Good y
  append (HasWarning _ wsx) (HasWarning y wsy) = HasWarning y (wsx <> wsy)
  append (HasWarning _ wsx) (Good y) = HasWarning y wsx
  append (Good _) (HasWarning y wsy) = HasWarning y wsy

instance showOk :: Show a => Show (Ok a) where
  show (Good x) = "Good " <> show x
  show (HasWarning x w) = "HasWarning " <> show x <> "; Warning: " <> show w
