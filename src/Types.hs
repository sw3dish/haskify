module Types where

-- Make it easy to write literal ByteString and Text values.
{-# LANGUAGE OverloadedStrings #-}

-- Our handy module.
import Network.Wreq

-- Operators such as (&) and (.~).
import Control.Lens

-- Conversion of Haskell values to JSON.
import Data.Aeson (toJSON)

-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)

import qualified Data.Text as T

import Data.Time.Clock.POSIX (POSIXTime)

data Token = Token {
  access_token :: T.Text
  , expiration_time :: POSIXTime
}
