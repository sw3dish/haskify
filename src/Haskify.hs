module Haskify where

import Network.Wreq

import Data.Aeson (decode, withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)
import Data.List
import Data.Monoid

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State.Lazy as State
import Control.Lens

import Types

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
  let options = defaults & header "Authorization" .~ ["Basic " <> B64.encode (clientId <> ":" <> secret)]
  r <- liftIO $  postWith options requestUrl ["grant_type" := ("client_credentials" :: String)]
  tok <-  haskifyLiftMaybe $ r ^? responseBody >>= decode
  State.put tok

-- /v1/albums/{id}
getAlbumSingle ::  String -> HaskifyAction Album
getAlbumSingle albumId = do
  auth <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums/" <> albumId
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/albums?ids={ids}
getAlbumMultiple :: [String] -> HaskifyAction [Album]
getAlbumMultiple albumIds = do
  auth <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums?ids=" <> intercalate "," albumIds
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ parseMaybe album_array =<< decode =<< (r ^? responseBody)

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
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features/" <> track_id
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/audio-features?ids={ids}
getAudioFeaturesMultiple :: [String] -> HaskifyAction [AudioFeatures]
getAudioFeaturesMultiple track_ids = do
  auth <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features?ids=" <> intercalate "," track_ids
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)

-- /v1/browse/featured-playlists

-- /v1/browse/new-releases
-- optional arguments that should be implemented: country, limit, offset
getNewReleases :: HaskifyAction NewReleasesResponse
getNewReleases = do
  auth <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/new-releases/"
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/categories

-- /v1/browse/categories/{id}

-- /v1/browse/categories/{id}/playlists

-- /v1/recommendations

-- /v1/tracks/{id}

-- /v1/tracks?ids={ids}

-- /v1/search
--TODO: Come up with a haskell encoding for the query string
search :: [SearchType] -> String -> HaskifyAction SearchResponse
search types query = do
  auth <- State.get
  let search_type = intercalate "," $ map searchTypeString types
  let requestUrl = apiUrlBase <> apiVersion <> "search?type=" <> search_type <> "&q=" <> query
  let options = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token auth)]
  r <- liftIO $ getWith options requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode
