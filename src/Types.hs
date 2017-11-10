module Types where

-- get FromJSON instances for free
{-# LANGUAGE DeriveGeneric #-}

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

import GHC.Generics

data Token = Token {
  access_token :: T.Text
  , expires_in :: POSIXTime
} deriving (Show)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \v -> Token
    <$> v .: "access_token"
    <*> v .: "expires_in"

data Album = Album {
   album_type :: T.Text
  ,album_artists    :: [ArtistSimplified]
  ,album_available_markets :: [T.Text]
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
    <*> (v .: "available_markets")
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

data ArtistSimplified = ArtistSimplified {
   artist_external_urls :: ExternalURL
  ,artist_href :: T.Text
  ,artist_id :: T.Text
  ,artist_name :: T.Text
  ,artist_obj_type :: T.Text -- artist
  ,artist_uri :: T.Text } deriving (Show)

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

data TrackSimplified = TrackSimplified {
   track_artists :: [ArtistSimplified]
  ,track_available_markets :: [T.Text]
  ,track_disc_number :: Integer
  ,track_duration_ms :: Integer -- Change to some time type?
  ,track_explicit :: Bool
  ,track_external_urls :: ExternalURL
  ,track_href :: T.Text
  ,track_id :: T.Text
  ,track_is_playable :: Maybe Bool -- only present when relinking applied
  ,track_linked_from :: Maybe TrackLink -- only present when relinking applied
  ,track_name :: T.Text
  ,track_preview_url :: Maybe T.Text
  ,track_track_number :: Integer
  ,track_obj_type :: T.Text -- "track"
  ,track_uri :: T.Text} deriving (Show)

instance FromJSON TrackSimplified where
  parseJSON = withObject "TrackSimplified" $ \v -> TrackSimplified
    <$> (v .: "artists")
    <*> (v .: "available_markets")
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
} deriving (Show, Generic)

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
