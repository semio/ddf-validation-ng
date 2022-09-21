module DDF.Validation.Types.Message where

-- this is the data type to display ValidationError object.

type Message =
  { message :: String
  , file :: String
  , lineNo :: Int
  , level :: String
  }

create :: String -> Message
create x =
  { message: x
  , file: ""
  , lineNo: 0
  , level: "log"
  }

-- TODO: add `warning` and `error` functions
