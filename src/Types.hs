module Types where

-- Make it easy to write literal ByteString and Text values.
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}

-- Our handy module.
import Network.Wreq

-- Operators such as (&) and (.~).
import Control.Lens

-- Conversion of Haskell values to JSON.
import Data.Aeson (toJSON, FromJSON, parseJSON, withObject, (.:), (.:?), (.!=), Value)

import Data.Aeson.Types
-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)

import qualified Data.Text as T

import Data.Time.Clock.POSIX (POSIXTime)

import Data.Maybe
import Data.Map
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.State.Lazy as State

type HaskifyAction = State.StateT Token (MaybeT IO)

haskifyLiftMaybe :: Maybe a -> HaskifyAction a
haskifyLiftMaybe = lift . MaybeT . return

data Token = Token {
  access_token :: T.Text
  , expires_in :: POSIXTime
} deriving (Show)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token
    <$> v .: "access_token"
    <*> v .: "expires_in"

type RequestParameter = (T.Text, T.Text)

data Album = Album {
   album_type :: AlbumType
  ,album_artists    :: [ArtistSimplified]
  ,album_available_markets :: Maybe [T.Text]
  ,album_copyrights :: [Copyright]
  ,album_external_ids :: ExternalID
  ,album_external_urls :: ExternalURL
  ,album_genres :: [T.Text]
  ,album_href :: T.Text
  ,album_id :: T.Text
  ,album_images :: [Image]
  ,album_label :: T.Text
  ,album_name :: T.Text
  ,album_popularity :: Integer
  ,album_release_date :: T.Text -- maybe parse to date object
  ,album_release_date_precision ::T.Text -- (year|month|day) would be used in parsing date
  ,album_tracks :: Paging TrackSimplified
  ,album_obj_type :: T.Text -- Should always be "album" include?
  ,album_uri :: T.Text} deriving (Show)

instance FromJSON Album where
  parseJSON = withObject "Album" $ \v -> Album
    <$> (v .: "album_type")
    <*> (v .: "artists")
    <*> (v .:? "available_markets")
    <*> (v .: "copyrights")
    <*> (v .: "external_ids")
    <*> (v .: "external_urls")
    <*> (v .: "genres")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .: "images")
    <*> (v .: "label")
    <*> (v .: "name")
    <*> (v .: "popularity")
    <*> (v .: "release_date")
    <*> (v .: "release_date_precision")
    <*> (v .: "tracks")
    <*> (v .: "type")
    <*> (v .: "uri")

-- This data structure duplicates a large amount of the
-- full album structre. We need to come up with some way to
-- encode this relationship.
data AlbumSimplified = AlbumSimplified {
   albumsimplified_type :: AlbumType
  ,albumsimplified_artists :: [ArtistSimplified]
  ,albumsimplified_available_markets :: Maybe [T.Text]
  ,albumsimplified_external_urls :: ExternalURL
  ,albumsimplified_href :: T.Text
  ,albumsimplified_id :: T.Text
  ,albumsimplified_images :: [Image]
  ,albumsimplified_name :: T.Text
  ,albumsimplified_obj_type :: T.Text -- album
  ,albumsimplified_uri :: T.Text} deriving (Show)

instance FromJSON AlbumSimplified where
  parseJSON = withObject "AlbumSimplified" $ \v -> AlbumSimplified
    <$> (v .: "album_type")
    <*> (v .: "artists")
    <*> (v .:? "available_markets")
    <*> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .: "images")
    <*> (v .: "name")
    <*> (v .: "type")
    <*> (v .: "uri")

data AlbumType = TypeAlbum | TypeSingle | TypeCompilation deriving (Show)

instance FromJSON AlbumType where
  parseJSON (String s) = pure $ makeAlbumType s

makeAlbumType :: T.Text -> AlbumType
makeAlbumType "album" = TypeAlbum
makeAlbumType "single" = TypeSingle
makeAlbumType "compilation" = TypeCompilation

album_array :: Value -> Parser [Album]
album_array = withObject "album_array" $ \o -> o .: "albums"


data Artist = Artist{
  artist_external_urls :: ExternalURL
  ,artist_followers :: Followers
  ,artist_genres :: [Genre]
  ,artist_href :: T.Text
  ,artist_id :: T.Text
  ,artist_images :: [Image]
  ,artist_name :: T.Text
  ,artist_popularity :: Integer
  ,artist_obj_type :: T.Text
  ,artist_uri :: T.Text
} deriving Show

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v -> Artist
    <$> (v.: "external_urls")
    <*> (v.: "followers")
    <*> (v.: "genres")
    <*> (v.: "href")
    <*> (v.: "id")
    <*> (v.: "images")
    <*> (v.: "name")
    <*> (v.: "popularity")
    <*> (v.: "type")
    <*> (v.: "uri")

artist_array :: Value -> Parser [Artist]
artist_array = withObject "artist_array" $ \o -> o .: "artists"

type Genre = T.Text

data Followers = Followers {
  followers_href :: Maybe T.Text
  ,followers_total :: Integer
} deriving (Show)

instance FromJSON Followers where
  parseJSON = withObject "Followers" $ \v -> Followers
    <$> (v .:? "href")
    <*> (v .: "total")

data ArtistSimplified = ArtistSimplified {
   artistsimplified_external_urls :: ExternalURL
  ,artistsimplified_href :: T.Text
  ,artistsimplified_id :: T.Text
  ,artistsimplified_name :: T.Text
  ,artistsimplified_obj_type :: T.Text -- artist
  ,artistsimplified_uri :: T.Text } deriving (Show)

instance FromJSON ArtistSimplified where
  parseJSON = withObject "ArtistSimplified" $ \v -> ArtistSimplified
    <$> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .: "name")
    <*> (v .: "type")
    <*> (v .: "uri")

data Copyright = Copyright {
   copyright_text :: T.Text
  ,copyright_obj_type :: T.Text} deriving (Show) -- C = copyright, P = performance copyright. Make enum?

instance FromJSON Copyright where
  parseJSON = withObject "Copyright" $ \v -> Copyright
    <$> (v .: "text")
    <*> (v .: "type")

type ExternalID = Map T.Text T.Text

type ExternalURL = Map T.Text T.Text

data Image = Image {
   image_height :: Maybe Integer
  ,image_width :: Maybe Integer
  ,image_url :: T.Text} deriving (Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v -> Image
    <$> (v .:? "height")
    <*> (v .:? "width")
    <*> (v .: "url")

data Paging a = Paging {
   paging_href :: T.Text
  ,paging_items :: [a]
  ,paging_limit :: Integer
  ,paging_next :: Maybe T.Text
  ,paging_offset :: Integer
  ,paging_previous :: Maybe T.Text
  ,paging_total :: Integer
} deriving (Show)

instance FromJSON a => FromJSON (Paging a) where
  parseJSON = withObject "Paging" $ \v -> Paging
    <$> (v .: "href")
    <*> (v .: "items")
    <*> (v .: "limit")
    <*> (v .:? "next")
    <*> (v .: "offset")
    <*> (v .:? "previous")
    <*> (v .: "total")

data Track = Track {
  track_album :: AlbumSimplified
  ,track_artists :: [ArtistSimplified]
  ,track_available_markets :: Maybe [T.Text]
  ,track_disc_number :: Integer
  ,track_duration_ms :: Integer
  ,track_explicit :: Bool
  ,track_external_ids :: ExternalID
  ,track_external_urls :: ExternalURL
  ,track_href :: T.Text
  ,track_id :: T.Text
  ,track_is_playable :: Maybe Bool
  ,track_linked_from :: Maybe TrackLink
  ,track_restrictions :: Maybe Restrictions
  ,track_name :: T.Text
  ,track_popularity :: Integer
  ,track_preview_url :: Maybe T.Text
  ,track_track_number :: Integer
  ,track_obj_type :: T.Text
  ,track_uri :: T.Text
} deriving (Show)

instance FromJSON Track where
  parseJSON = withObject "Track" $ \v -> Track
    <$> (v .: "album")
    <*> (v .: "artists")
    <*> (v .:? "available_markets")
    <*> (v .: "disc_number")
    <*> (v .: "duration_ms")
    <*> (v .: "explicit")
    <*> (v .: "external_ids")
    <*> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .:? "is_playable")
    <*> (v .:? "linked_from")
    <*> (v .:? "restrictions")
    <*> (v .: "name")
    <*> (v .: "popularity")
    <*> (v .:? "preview_url")
    <*> (v .: "track_number")
    <*> (v .: "type")
    <*> (v .: "uri")

track_array :: Value -> Parser [Track]
track_array = withObject "track_array" $ \o -> o .: "tracks"

data TrackSimplified = TrackSimplified {
   tracksimplified_artists :: [ArtistSimplified]
  ,tracksimplified_available_markets :: Maybe [T.Text]
  ,tracksimplified_disc_number :: Integer
  ,tracksimplified_duration_ms :: Integer -- Change to some time type?
  ,tracksimplified_explicit :: Bool
  ,tracksimplified_external_urls :: ExternalURL
  ,tracksimplified_href :: T.Text
  ,tracksimplified_id :: T.Text
  ,tracksimplified_is_playable :: Maybe Bool -- only present when relinking applied
  ,tracksimplified_linked_from :: Maybe TrackLink -- only present when relinking applied
  ,tracksimplified_name :: T.Text
  ,tracksimplified_preview_url :: Maybe T.Text
  ,tracksimplified_track_number :: Integer
  ,tracksimplified_obj_type :: T.Text -- "track"
  ,tracksimplified_uri :: T.Text} deriving (Show)

instance FromJSON TrackSimplified where
  parseJSON = withObject "TrackSimplified" $ \v -> TrackSimplified
    <$> (v .: "artists")
    <*> (v .:? "available_markets")
    <*> (v .: "disc_number")
    <*> (v .: "duration_ms")
    <*> (v .: "explicit")
    <*> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .:? "is_playable")
    <*> (v .:? "linked_from")
    <*> (v .: "name")
    <*> (v .:? "preview_url")
    <*> (v .: "track_number")
    <*> (v .: "type")
    <*> (v .: "uri")

data TrackLink = TrackLink {
   tracklink_external_urls :: ExternalURL
  ,tracklink_href :: T.Text
  ,tracklink_id :: T.Text
  ,tracklink_obj_type :: T.Text -- "track"
  ,tracklink_url :: T.Text} deriving (Show)

instance FromJSON TrackLink where
  parseJSON = withObject "TrackLink" $ \v -> TrackLink
    <$> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .: "type")
    <*> (v .: "uri")

type Restrictions = Map T.Text T.Text -- Documentation is unclear how this object is formed

data AudioFeatures = AudioFeatures {
  audiofeatures_danceability :: Float
  ,audiofeatures_energy :: Float
  ,audiofeatures_key :: Integer
  ,audiofeatures_loudness :: Float
  ,audiofeatures_mode :: Integer
  ,audiofeatures_speechiness :: Float
  ,audiofeatures_acousticness :: Float
  ,audiofeatures_instrumentalness :: Float
  ,audiofeatures_liveness :: Float
  ,audiofeatures_valence :: Float
  ,audiofeatures_tempo :: Float
  ,audiofeatures_obj_type :: T.Text -- audio_features
  ,audiofeatures_id :: T.Text
  ,audiofeatures_uri :: T.Text
  ,audiofeatures_track_href :: T.Text
  ,audiofeatures_analysis_url :: T.Text
  ,audiofeatures_duration_ms :: Integer
  ,audiofeatures_time_signature :: Integer
} deriving (Show)

instance FromJSON AudioFeatures where
  parseJSON = withObject "audiofeatures" $ \o -> do
    audiofeatures_danceability <- o .: "danceability"
    audiofeatures_energy <- o .: "energy"
    audiofeatures_key <- o .: "key"
    audiofeatures_loudness <- o .: "loudness"
    audiofeatures_mode <- o .: "mode"
    audiofeatures_speechiness <- o .: "speechiness"
    audiofeatures_acousticness <- o .: "acousticness"
    audiofeatures_instrumentalness <- o .: "instrumentalness"
    audiofeatures_liveness <- o .: "liveness"
    audiofeatures_valence <- o .: "valence"
    audiofeatures_tempo <- o .: "tempo"
    audiofeatures_obj_type <- o .: "type"
    audiofeatures_id <- o .: "id"
    audiofeatures_uri <- o .: "uri"
    audiofeatures_track_href <- o .: "track_href"
    audiofeatures_analysis_url <- o .: "analysis_url"
    audiofeatures_duration_ms <- o .: "duration_ms"
    audiofeatures_time_signature <- o .: "time_signature"

    return $ AudioFeatures
      audiofeatures_danceability audiofeatures_energy
      audiofeatures_key audiofeatures_loudness audiofeatures_mode
      audiofeatures_speechiness audiofeatures_acousticness audiofeatures_instrumentalness
      audiofeatures_liveness audiofeatures_valence audiofeatures_tempo
      audiofeatures_obj_type audiofeatures_id audiofeatures_uri
      audiofeatures_track_href audiofeatures_analysis_url
      audiofeatures_duration_ms audiofeatures_time_signature

audiofeatures_array :: Value -> Parser [AudioFeatures]
audiofeatures_array = withObject "audiofeatures_array" $ \o -> o .: "audio_features"


data PlaylistSimplified = PlaylistSimplified {
  playlistsimplified_collaborative :: Bool
  ,playlistsimplified_external_urls :: ExternalURL
  ,playlistsimplified_href :: T.Text
  ,playlistsimplified_id :: T.Text
  ,playlistsimplified_images :: [Image]
  ,playlistsimplified_name :: T.Text
  ,playlistsimplified_owner :: UserPublic
  ,playlistsimplified_public :: Maybe Bool
  ,playlistsimplified_snapshot_id :: T.Text
  ,playlistsimplified_tracks :: Tracks
  ,playlistsimplified_type :: T.Text
  ,playlistsimplified_uri :: T.Text
} deriving (Show)

instance FromJSON PlaylistSimplified where
  parseJSON = withObject "PlaylistSimplified" $ \v -> PlaylistSimplified
    <$> (v .: "collaborative")
    <*> (v .: "external_urls")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .: "images")
    <*> (v .: "name")
    <*> (v .: "owner")
    <*> (v .:? "public")
    <*> (v .: "snapshot_id")
    <*> (v .: "tracks")
    <*> (v .: "type")
    <*> (v .: "uri")

data Tracks = Tracks {
  tracks_href :: Maybe T.Text
  ,tracks_total :: Integer
} deriving (Show)

instance FromJSON Tracks where
  parseJSON = withObject "Tracks" $ \v -> Tracks
    <$> (v .:? "href")
    <*> (v .: "total")

data UserPublic = UserPublic {
  userpublic_display_name :: Maybe T.Text
  ,userpublic_external_urls :: ExternalURL
  ,userpublic_followers :: Maybe Followers -- the Spotify user does not have this field
  ,userpublic_href :: T.Text
  ,userpublic_id :: T.Text
  ,userpublic_images :: Maybe [Image] -- the Spotify user does not have this field (this should be reported to the API developers)
  ,userpublic_type :: T.Text
  ,userpublic_uri :: T.Text
} deriving (Show)

instance FromJSON UserPublic where
  parseJSON = withObject "UserPublic" $ \v -> UserPublic
    <$> (v .:? "display_name")
    <*> (v .: "external_urls")
    <*> (v .:? "followers")
    <*> (v .: "href")
    <*> (v .: "id")
    <*> (v .:? "images")
    <*> (v .: "type")
    <*> (v .: "uri")

data Category = Category {
  category_href :: T.Text
  ,category_icons :: [Image]
  ,category_id :: T.Text
  ,category_name :: T.Text
} deriving (Show)

instance FromJSON Category where
  parseJSON = withObject "Category" $ \v -> Category
    <$> (v .: "href")
    <*> (v .: "icons")
    <*> (v .: "id")
    <*> (v .: "name")

data SearchType = SearchTypeAlbum | SearchTypeArtist | SearchTypeTrack deriving (Show, Enum, Bounded)

searchTypeString SearchTypeAlbum = "album"
searchTypeString SearchTypeArtist = "artist"
searchTypeString SearchTypeTrack = "track"


-- The API spec says that the message should always be present. In  practice, it seems to never be present.
-- This field could be removed if we can confirm that no message is given in response.
-- It might be worth reporting this as an issue to the developers of the API.
newtype NewReleasesResponse = NewReleasesResponse (Maybe T.Text, Paging AlbumSimplified) deriving (Show)

instance FromJSON NewReleasesResponse where
  parseJSON = withObject "NewReleasesResponse" $ \v -> do
    message <- v .:? "message"
    content <- v .: "albums"
    return $ NewReleasesResponse (message, content)

newtype FeaturedPlaylistsResponse = FeaturedPlaylistsResponse (Maybe T.Text, Paging PlaylistSimplified) deriving (Show)

instance FromJSON FeaturedPlaylistsResponse where
  parseJSON = withObject "FeaturedPlaylistsResponse" $ \v -> do
    message <- v .:? "message"
    content <- v .: "playlists"
    return $ FeaturedPlaylistsResponse (message, content)

newtype CategoriesResponse = CategoriesResponse (Paging Category) deriving (Show)

instance FromJSON CategoriesResponse where
  parseJSON = withObject "CategoriesResponse" $ \v -> do
    content <- v .: "categories"
    return $ CategoriesResponse (content)

newtype CategoryPlaylistsResponse = CategoryPlaylistsResponse (Paging PlaylistSimplified) deriving (Show)

instance FromJSON CategoryPlaylistsResponse where
  parseJSON = withObject "CategoryPlaylistsResponse" $ \v -> do
    content <- v .: "playlists"
    return $ CategoryPlaylistsResponse (content)

newtype SearchResponse = SearchResponse (Maybe (Paging Artist), Maybe (Paging AlbumSimplified), Maybe (Paging Track)) deriving (Show)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \v -> do
    artists <- v .:? "artists"
    albums  <- v .:? "albums"
    tracks  <- v .:? "tracks"
    return $ SearchResponse (artists, albums, tracks)
