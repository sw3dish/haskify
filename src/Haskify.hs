module Haskify where

import Network.Wreq

import Data.Aeson (Value(Object), ToJSON, FromJSON, decode, encode)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Lazy as State
import Control.Lens
import Control.Applicative

import Types

apiUrlBase, apiVersion :: String
apiUrlBase = "https://api.spotify.com/"
apiVersion = "v1/"

tokenUrlBase :: String
tokenUrlBase = "https://accounts.spotify.com/"

haskifyDefaultOptions :: Token -> Options
haskifyDefaultOptions token = defaults & header "Authorization".~ ["Bearer " <> encodeUtf8 (access_token token)]

haskifyGetEndpoint :: Token -> [RequestParameter] -> String -> IO (Response BL.ByteString)
haskifyGetEndpoint token optionalParameters = getWith ((haskifyDefaultOptions token) & params .~ optionalParameters)

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
getAlbumSingle ::  String -> [RequestParameter] -> HaskifyAction Album
getAlbumSingle albumId optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums/" <> albumId
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/albums?ids={ids}
getAlbumMultiple :: [String] -> [RequestParameter] -> HaskifyAction [Album]
getAlbumMultiple albumIds optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums?ids=" <> intercalate "," albumIds
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ parseMaybe album_array =<< decode =<< (r ^? responseBody)

-- /v1/albums/{id}/tracks
getAlbumTracks :: String -> [RequestParameter] -> HaskifyAction (Paging TrackSimplified)
getAlbumTracks albumId optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "albums/" <> albumId <> "/tracks"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists/{id}
getArtistSingle :: String -> HaskifyAction Artist
getArtistSingle artistId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists?ids={ids}
getArtistMultiple :: [String] -> HaskifyAction [Artist]
getArtistMultiple artistIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists?ids=" <> intercalate "," artistIds
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ parseMaybe artist_array =<< decode =<< (r ^? responseBody)

-- /v1/artists/{id}/albums
getArtistAlbums :: String -> [RequestParameter] -> HaskifyAction (Paging AlbumSimplified)
getArtistAlbums artistId optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/albums"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/artists/{id}/top-tracks?country={country}
-- country is required
getArtistTopTracks :: String -> String -> HaskifyAction [Track]
getArtistTopTracks artistId country = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/top-tracks?country=" <> country
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ parseMaybe track_array =<< decode =<< (r ^? responseBody)

-- /v1/artists/{id}/related-artists
getArtistRelatedArtists :: String -> HaskifyAction [Artist]
getArtistRelatedArtists artistId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "artists/" <> artistId <> "/related-artists"
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ parseMaybe artist_array =<< decode =<< (r ^? responseBody)

-- /v1/audio-analysis/{id}

-- /v1/audio-features/{id}
getAudioFeaturesSingle :: String ->  HaskifyAction AudioFeatures
getAudioFeaturesSingle trackId = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features/" <> trackId
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/audio-features?ids={ids}
getAudioFeaturesMultiple :: [String] -> HaskifyAction [AudioFeatures]
getAudioFeaturesMultiple trackIds = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "audio-features?ids=" <> intercalate "," trackIds
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  haskifyLiftMaybe $ parseMaybe audiofeatures_array =<< decode =<< (r ^? responseBody)

-- /v1/browse/featured-playlists
getFeaturedPlaylists :: [RequestParameter] -> HaskifyAction FeaturedPlaylistsResponse
getFeaturedPlaylists optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/featured-playlists/"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  (FeaturedPlaylistsResponse (msg, paging)) <- haskifyLiftMaybe $ r ^? responseBody >>= decode
  return $ FeaturedPlaylistsResponse (msg, paging {_paging_json_wrapper = Just "playlists"})

-- /v1/browse/new-releases
-- optional arguments that should be implemented: country, limit, offset
getNewReleases :: [RequestParameter] -> HaskifyAction NewReleasesResponse
getNewReleases optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/new-releases/"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  (NewReleasesResponse (msg, paging)) <- haskifyLiftMaybe $ r ^? responseBody >>= decode
  return $ NewReleasesResponse (msg, paging {_paging_json_wrapper = Just "albums"})

-- /v1/browse/categories
getCategoryMultiple :: [RequestParameter] -> HaskifyAction CategoriesResponse
getCategoryMultiple optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  (CategoriesResponse paging) <- haskifyLiftMaybe $ r ^? responseBody >>= decode
  return $ CategoriesResponse (paging {_paging_json_wrapper = Just "categories"})

-- /v1/browse/categories/{id}
getCategorySingle :: String -> [RequestParameter] -> HaskifyAction Category
getCategorySingle categoryId optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/browse/categories/{id}/playlists
getCategoryPlaylists :: String -> [RequestParameter] -> HaskifyAction CategoryPlaylistsResponse
getCategoryPlaylists categoryId optionalParameters  = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "browse/categories/" <> categoryId <> "/playlists"
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  (CategoryPlaylistsResponse paging) <- haskifyLiftMaybe $ r ^? responseBody >>= decode
  return $ CategoryPlaylistsResponse paging {_paging_json_wrapper = Just "playlists"}

-- /v1/recommendations

-- /v1/tracks/{id}
getTrackSingle :: String -> [RequestParameter] -> HaskifyAction Track
getTrackSingle trackId optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "tracks/" <> trackId
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

-- /v1/tracks?ids={ids}
getTrackMultiple :: [String] -> [RequestParameter] -> HaskifyAction [Track]
getTrackMultiple trackIds optionalParameters = do
  token <- State.get
  let requestUrl = apiUrlBase <> apiVersion <> "tracks?ids=" <> intercalate "," trackIds
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ parseMaybe track_array =<< decode =<< (r ^? responseBody)

-- /v1/search
--TODO: Come up with a haskell encoding for the query string
search :: [SearchType] -> String -> [RequestParameter] -> HaskifyAction SearchResponse
search types query optionalParameters = do
  token <- State.get
  let search_type = intercalate "," $ map searchTypeString types
  let requestUrl = apiUrlBase <> apiVersion <> "search?type=" <> search_type <> "&q=" <> query
  r <- liftIO $ haskifyGetEndpoint token optionalParameters requestUrl
  haskifyLiftMaybe $ r ^? responseBody >>= decode

getPagingNext,getPagingPrevious :: FromJSON a => Paging a -> HaskifyAction (Paging a)
getPagingNext page = do
  next <- haskifyLiftMaybe $ paging_next page
  getPaging (_paging_json_wrapper page) . T.unpack $ next
getPagingPrevious page = do
  prev <- haskifyLiftMaybe $ paging_previous page
  getPaging (_paging_json_wrapper page) . T.unpack $ prev

getPaging :: FromJSON a => Maybe T.Text -> String -> HaskifyAction (Paging a)
getPaging wrapper requestUrl = do
  token <- State.get
  r <- liftIO $ haskifyGetEndpoint token [] requestUrl
  (<|>)
    (do
      justWrapper <- haskifyLiftMaybe wrapper
      (Object value) <- haskifyLiftMaybe $  r ^? responseBody >>= decode
      pageValue <- haskifyLiftMaybe $ M.lookup justWrapper value
      -- not a good way to do this. I want to directly decode something with type Value but I can't figure out
      -- how to do that. Instead, I encode it as a JSON ByteString (encode) then decode the byte string to
      -- get what I want (decode).
      haskifyLiftMaybe (decode . encode $ pageValue))
    (do
      haskifyLiftMaybe $ r ^? responseBody >>= decode)

collectPaging :: FromJSON a => Paging a -> HaskifyAction [a]
collectPaging page = ((paging_items page)++) <$> ((getPagingNext page >>= collectPaging) <|> (return []))
