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
import qualified Data.ByteString as B
import Data.Monoid

import Types

import Data.Text as T

import Data.List as L

-- Convert from text to bytestring
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (toJSON, FromJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (parseMaybe)

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

apiUrlBase, apiVersion :: String
apiUrlBase = "https://api.spotify.com/"
apiVersion = "v1/"

authUrlBase :: String
authUrlBase = "https://accounts.spotify.com/"

-- Request an auth token from the spotify api
requestToken :: B.ByteString -> B.ByteString -> MaybeT IO Token
requestToken clientId secret = do
  let requestUrl = authUrlBase <> "api/token"
  let options = defaults & header "Authorization" .~ ["Basic " <> (B64.encode $ clientId <> ":" <> secret)]
  r <- liftIO $ postWith options requestUrl ["grant_type" := ("client_credentials" :: String)]
  MaybeT . return $ r ^? responseBody >>= decode

getAlbumSingle :: Token -> String -> MaybeT IO Album
getAlbumSingle auth albumId = do
  let requestUrl = (apiUrlBase <> apiVersion <> "albums/" <> albumId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  MaybeT . return $ r ^? responseBody >>= decode

getAlbumMultiple :: Token -> [String] -> MaybeT IO [Album]
getAlbumMultiple auth albumIds = do
  let requestUrl = (apiUrlBase <> apiVersion <> "albums?ids=" <> (L.intercalate "," albumIds))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  MaybeT . return $ ((parseMaybe album_array =<< decode =<< (r ^? responseBody)) :: Maybe [Album])

getAudioFeaturesSingle :: Token -> String ->  MaybeT IO AudioFeatures
getAudioFeaturesSingle auth track_id = do
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features/" <> track_id)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  MaybeT . return $ r ^? responseBody >>= decode

getAudioFeaturesMultiple :: Token -> [String] -> MaybeT IO [AudioFeatures]
getAudioFeaturesMultiple auth track_ids = do
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features?ids=" <> (L.intercalate "," track_ids))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  MaybeT . return $ ((parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)) :: Maybe [AudioFeatures])

-- optional arguments that should be implemented: country, limit, offset
getNewReleases :: Token -> MaybeT IO NewReleasesResponse
getNewReleases auth = do
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/new-releases/")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  MaybeT . return $ r ^? responseBody >>= decode
