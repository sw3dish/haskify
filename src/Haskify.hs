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
import qualified Control.Monad.Trans.State.Lazy as State

apiUrlBase, apiVersion :: String
apiUrlBase = "https://api.spotify.com/"
apiVersion = "v1/"

authUrlBase :: String
authUrlBase = "https://accounts.spotify.com/"

-- Request an auth token from the spotify api
-- Injects new auth token into state monad
requestToken :: B.ByteString -> B.ByteString -> HaskifyAction ()
requestToken clientId secret = do
  let requestUrl = authUrlBase <> "api/token"
  let options = defaults & header "Authorization" .~ ["Basic " <> (B64.encode $ clientId <> ":" <> secret)]
  r <- liftIO $  postWith options requestUrl ["grant_type" := ("client_credentials" :: String)]
  tok <-  lift . MaybeT . return $ r ^? responseBody >>= decode
  State.put tok

-- /v1/albums/{id}
getAlbumSingle ::  String -> HaskifyAction Album
getAlbumSingle albumId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "albums/" <> albumId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/albums?ids={ids}
getAlbumMultiple :: [String] -> HaskifyAction [Album]
getAlbumMultiple albumIds = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "albums?ids=" <> (L.intercalate "," albumIds))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe album_array =<< decode =<< (r ^? responseBody)) :: Maybe [Album])

-- /v1/albums/{id}/tracks

-- /v1/artists/{id}

-- /v1/artists?ids={ids}

-- /v1/artists/{id}/albums

-- /v1/artists/{id}/related-artists

-- /v1/audio-analysis/{id}

-- /v1/audio-features/{id}
getAudioFeaturesSingle :: String ->  HaskifyAction AudioFeatures
getAudioFeaturesSingle track_id = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features/" <> track_id)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/audio-features?ids={ids}
getAudioFeaturesMultiple :: [String] -> HaskifyAction [AudioFeatures]
getAudioFeaturesMultiple track_ids = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features?ids=" <> (L.intercalate "," track_ids))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)) :: Maybe [AudioFeatures])

-- /v1/browse/featured-playlists

-- /v1/browse/new-releases
-- optional arguments that should be implemented: country, limit, offset
getNewReleases :: HaskifyAction NewReleasesResponse
getNewReleases = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/new-releases/")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/browse/categories

-- /v1/browse/categories/{id}

-- /v1/browse/categories/{id}/playlists

-- /v1/recommendations

-- /v1/tracks/{id}

-- /v1/tracks?ids={ids}

-- /v1/search
--TODO: Come up with a haskell encoding for the query string
searchAlbum :: String -> HaskifyAction (Paging AlbumSimplified)
searchAlbum query = do
  auth <- State.get
  let search_type = "album" -- replace wih string generated from enumartion type
  let requestUrl = apiUrlBase <> apiVersion <> "search?type=" <> search_type <> "&q=" <> query
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode
