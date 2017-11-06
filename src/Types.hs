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
import Data.Aeson (toJSON, FromJSON, parseJSON, withObject, (.:), (.:?), (.!=))

-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)

import qualified Data.Text as T

import Data.Time.Clock.POSIX (POSIXTime)

import Data.Maybe

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
  ,album_uri :: T.Text } deriving (Show, Generic)

instance FromJSON Album

data ArtistSimplified = ArtistSimplified {
   artist_external_urls :: ExternalURL
  ,artist_href :: T.Text
  ,artist_id :: T.Text
  ,artist_name :: T.Text
  ,artist_obj_type :: T.Text -- artist
  ,artist_uri :: T.Text } deriving (Show, Generic)

instance FromJSON ArtistSimplified

data Copyright = Copyright {
   copyright_text :: T.Text
  ,copyright_obj_type :: T.Text} deriving (Show, Generic) -- C = copyright, P = performance copyright. Make enum?

instance FromJSON Copyright

type ExternalID = (T.Text, T.Text)

type ExternalURL = (T.Text, T.Text)

data Image = Image {
   image_height :: Maybe Integer
  ,image_width :: Maybe Integer
  ,image_url :: T.Text} deriving (Show, Generic)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v -> Image
    <$> (v .:? "height" .!= Nothing)
    <*> (v .:? "width" .!= Nothing)
    <*> (v .: "url")

data Paging a = Paging {
   paging_href :: T.Text
  ,paging_items :: [a]
  ,paging_limit :: Integer
  ,paging_next :: Maybe T.Text
  ,paging_offset :: Integer
  ,paging_previous :: Maybe T.Text
  ,paging_total :: Integer} deriving (Show, Generic)

instance FromJSON a => FromJSON (Paging a)

data TrackSimplified = TrackSimplified {
   track_artists :: [ArtistSimplified]
  ,track_available_markets :: [T.Text]
  ,track_disc_number :: Integer
  ,track_duration_ms :: Integer -- Change to some time type?
  ,track_explicit :: Bool
  ,track_external_urls :: ExternalURL
  ,track_href :: T.Text
  ,track_is_playable :: Bool
  ,track_linked_from :: TrackLink
  ,track_name :: T.Text
  ,track_preview_url :: Maybe T.Text
  ,track_track_number :: Integer
  ,track_obj_type :: T.Text -- "track"
  ,track_uri :: T.Text} deriving (Show, Generic)

instance FromJSON TrackSimplified

data TrackLink = TrackLink {
   tracklink_external_urls :: ExternalURL
  ,tracklink_href :: T.Text
  ,tracklink_id :: T.Text
  ,tracklink_obj_type :: T.Text -- "track"
  ,tracklink_url :: T.Text} deriving (Show, Generic)

instance FromJSON TrackLink

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
