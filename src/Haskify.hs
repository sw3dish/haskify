module Haskify where

-- Recommended Imports and Extensions for Wreq

-- Make it easy to write literal ByteString and Text values.
{-# LANGUAGE OverloadedStrings #-}

-- Our handy module.
import Network.Wreq

-- Operators such as (&) and (.~).
import Control.Lens

-- Conversion of Haskell values to JSON.
import Data.Aeson (toJSON, decode, Value)

-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)

-- End recommended imports

-- Encode strings in b64 as required by authorization api
import qualified Data.ByteString.Base64 as B64 (encode)

import Data.Monoid

import Types

import Data.Text as T

-- Convert from text to bytestring
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (toJSON, FromJSON, parseJSON, withObject, (.:))

apiUrlBase, apiVersion :: String
apiUrlBase = "https://api.spotify.com/"
apiVersion = "v1/"

authUrlBase :: String
authUrlBase = "https://accounts.spotify.com/"

-- Request an auth token from the spotify api
requestToken clientId secret = do
  let requestUrl = authUrlBase <> "api/token"
  let options = defaults & header "Authorization" .~ ["Basic " <> (B64.encode $ clientId <> ":" <> secret)]
  r <- postWith options requestUrl ["grant_type" := ("client_credentials" :: String)]
  return $   ((r ^? responseBody >>= decode) :: Maybe Token)

getAlbum auth albumId = do
  let requestUrl = (apiUrlBase <> apiVersion <> "albums/" <> albumId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- getWith options requestUrl
  return $ ((r ^? responseBody >>= decode) :: Maybe Album)
  --return $ ((r ^? responseBody >>= decode) :: Maybe Value)
