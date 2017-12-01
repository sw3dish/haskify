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
getAlbumTracks :: String -> HaskifyAction (Paging TrackSimplified)
getAlbumTracks albumId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "albums/" <> albumId <> "/tracks")
  let options = defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/artists/{id}
getArtistSingle :: String -> HaskifyAction Artist
getArtistSingle artistId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "artists/" <> artistId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/artists?ids={ids}
getArtistMultiple :: [String] -> HaskifyAction [Artist]
getArtistMultiple artistIds = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "artists?ids=" <> (L.intercalate "," artistIds))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe artist_array =<< decode =<< (r ^? responseBody)) :: Maybe [Artist])

-- /v1/artists/{id}/albums
getArtistAlbums :: String -> HaskifyAction (Paging AlbumSimplified)
getArtistAlbums artistId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/albums")
  let options = defaults & header "Authorization" .~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/artists/{id}/top-tracks?country={country}
-- country is required
getArtistTopTracks :: String -> String -> HaskifyAction [Track]
getArtistTopTracks artistId country = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/top-tracks?country=" <> country)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe track_array =<< decode =<< (r ^? responseBody)) :: Maybe [Track])

-- /v1/artists/{id}/related-artists
getArtistRelatedArtists :: String -> HaskifyAction [Artist]
getArtistRelatedArtists artistId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/related-artists")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe artist_array =<< decode =<< (r ^? responseBody)) :: Maybe [Artist])

-- /v1/audio-analysis/{id}

-- /v1/audio-features/{id}
getAudioFeaturesSingle :: String ->  HaskifyAction AudioFeatures
getAudioFeaturesSingle trackId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features/" <> trackId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/audio-features?ids={ids}
getAudioFeaturesMultiple :: [String] -> HaskifyAction [AudioFeatures]
getAudioFeaturesMultiple trackIds = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "audio-features?ids=" <> (L.intercalate "," trackIds))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)) :: Maybe [AudioFeatures])

-- /v1/browse/featured-playlists
getFeaturedPlaylists :: HaskifyAction FeaturedPlaylistsResponse
getFeaturedPlaylists = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/featured-playlists/")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

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
getCategoryMultiple :: HaskifyAction CategoriesResponse
getCategoryMultiple = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/categories/")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/browse/categories/{id}
getCategorySingle :: String -> HaskifyAction Category
getCategorySingle categoryId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/browse/categories/{id}/playlists
getCategoryPlaylists :: String -> HaskifyAction CategoryPlaylistsResponse
getCategoryPlaylists categoryId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId <> "/playlists")
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/recommendations

-- /v1/tracks/{id}
getTrackSingle :: String -> HaskifyAction Track
getTrackSingle trackId = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "tracks/" <> trackId)
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ r ^? responseBody >>= decode

-- /v1/tracks?ids={ids}
getTrackMultiple :: [String] -> HaskifyAction [Track]
getTrackMultiple trackIds = do
  auth <- State.get
  let requestUrl = (apiUrlBase <> apiVersion <> "tracks?ids=" <> (L.intercalate "," trackIds))
  let options = defaults & header "Authorization".~ ["Bearer " <> (encodeUtf8 $ access_token auth)]
  r <- liftIO $ getWith options requestUrl
  lift . MaybeT . return $ ((parseMaybe track_array =<< decode =<< (r ^? responseBody)) :: Maybe [Track])
