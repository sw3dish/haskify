module Haskify where

import Network.Wreq

import Data.Aeson (FromJSON, decode)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.List
import Data.Monoid
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Lazy as State
import Control.Lens

import Types

apiUrlBase, apiVersion :: String
apiUrlBase = "https://api.spotify.com/"
apiVersion = "v1/"

tokenUrlBase :: String
tokenUrlBase = "https://accounts.spotify.com/"

haskifyDefaultOptions :: Token -> Options
haskifyDefaultOptions token = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token token)]

haskifyGetEndpoint :: Token -> String -> IO (Response BL.ByteString)
haskifyGetEndpoint token = getWith (haskifyDefaultOptions token)

-- Request an token token from the spotify api
-- Injects new token token into state monad
requestToken :: B.ByteString -> B.ByteString -> HaskifyAction ()
requestToken clientId secret = do
  let requestUrl = tokenUrlBase <> "api/token"
  let opts = defaults & header "Authorization" .~ ["Basic " <> B64.encode (clientId <> ":" <> secret)]
  r <- liftIO $  postWith opts requestUrl ["grant_type" := ("client_credentials" :: String)]
  tok <-  haskifyLiftMaybe $ r ^? responseBody >>= decode
  State.put tok

-- /v1/albums/{id}
getAlbumSingle ::  String -> HaskifyAction Album
getAlbumSingle albumId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums/" <> albumId
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/albums?ids={ids}
getAlbumMultiple :: [String] -> HaskifyAction [Album]
getAlbumMultiple albumIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums?ids=" <> intercalate "," albumIds
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe album_array =<< decode =<< (r ^? responseBody)

-- /v1/albums/{id}/tracks
getAlbumTracks :: String -> HaskifyAction (Paging TrackSimplified)
getAlbumTracks albumId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums/" <> albumId <> "/tracks"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists/{id}
getArtistSingle :: String -> HaskifyAction Artist
getArtistSingle artistId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists?ids={ids}
getArtistMultiple :: [String] -> HaskifyAction [Artist]
getArtistMultiple artistIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists?ids=" <> intercalate "," artistIds
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe artist_array =<< decode =<< (r ^? responseBody) 

-- /v1/artists/{id}/albums
getArtistAlbums :: String -> HaskifyAction (Paging AlbumSimplified)
getArtistAlbums artistId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/albums"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists/{id}/top-tracks?country={country}
-- country is required
getArtistTopTracks :: String -> String -> HaskifyAction [Track]
getArtistTopTracks artistId country = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/top-tracks?country=" <> country
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe track_array =<< decode =<< (r ^? responseBody)

-- /v1/artists/{id}/related-artists
getArtistRelatedArtists :: String -> HaskifyAction [Artist]
getArtistRelatedArtists artistId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/related-artists"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe artist_array =<< decode =<< (r ^? responseBody)

-- /v1/audio-analysis/{id}

-- /v1/audio-features/{id}
getAudioFeaturesSingle :: String ->  HaskifyAction AudioFeatures
getAudioFeaturesSingle trackId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features/" <> trackId
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/audio-features?ids={ids}
getAudioFeaturesMultiple :: [String] -> HaskifyAction [AudioFeatures]
getAudioFeaturesMultiple trackIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features?ids=" <> intercalate "," trackIds
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)

-- /v1/browse/featured-playlists
getFeaturedPlaylists :: HaskifyAction FeaturedPlaylistsResponse
getFeaturedPlaylists = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/featured-playlists/"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/new-releases
-- optional arguments that should be implemented: country, limit, offset
getNewReleases :: HaskifyAction NewReleasesResponse
getNewReleases = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/new-releases/"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/categories
getCategoryMultiple :: HaskifyAction CategoriesResponse
getCategoryMultiple = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/categories/{id}
getCategorySingle :: String -> HaskifyAction Category
getCategorySingle categoryId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/categories/{id}/playlists
getCategoryPlaylists :: String -> HaskifyAction CategoryPlaylistsResponse
getCategoryPlaylists categoryId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId <> "/playlists"
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/recommendations

-- /v1/tracks/{id}
getTrackSingle :: String -> HaskifyAction Track
getTrackSingle trackId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "tracks/" <> trackId
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/tracks?ids={ids}
getTrackMultiple :: [String] -> HaskifyAction [Track]
getTrackMultiple trackIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "tracks?ids=" <> intercalate "," trackIds
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ parseMaybe track_array =<< decode =<< (r ^? responseBody)

-- /v1/search
--TODO: Come up with a haskell encoding for the query string
search :: [SearchType] -> String -> HaskifyAction SearchResponse
search types query = do
  token <- State.get
  let search_type = intercalate "," $ map searchTypeString types
  let requestUrl = apiUrlBase <> apiVersion <> "search?type=" <> search_type <> "&q=" <> query
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

getPagingNext,getPagingPrevious :: FromJSON a => Paging a -> HaskifyAction (Paging a)
getPagingNext page = do
  next <- haskifyLiftMaybe $ paging_next page
  getPaging . T.unpack $ next
getPagingPrevious page = do
  prev <- haskifyLiftMaybe $ paging_previous page
  getPaging . T.unpack $ prev

getPaging :: FromJSON a => String -> HaskifyAction (Paging a)
getPaging requestUrl = do
  token <- State.get
  r <- liftIO $ haskifyGetEndpoint token requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode
