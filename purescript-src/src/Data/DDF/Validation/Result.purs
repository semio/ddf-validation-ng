module Data.DDF.Validation.Result where

-- import Prelude

-- | message that shows the actual error for some validation.
newtype Error = Error String  -- TODO: use GADT for errors.

type Errors = Array Error


-- | message that contains more context information.
type Message
  = { message :: Errors
    , file :: String
    , lineNo :: Int
    }

