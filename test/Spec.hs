{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Data.Maybe (fromJust)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  testRequestToken
  testGetAlbum
  testGetAudioFeaturesSingle
  testGetAudioFeaturesMultiple

testRequestToken = requestToken testClientId testClientSecret >>= print

testGetAlbum = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  auth <- requestToken testClientId testClientSecret
  album <- join <$> (sequence $ getAlbum <$> auth <*> (pure album_id))
  print album

testGetAudioFeaturesSingle = do
  let track_id = "1ZLfI1KqHS2JFP7lKsC8bl" :: String
  auth <- fromJust <$> requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesSingle auth track_id
  print audio_features

testGetAudioFeaturesMultiple = do
  let track_ids = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"] :: [String]
  auth <- fromJust <$> requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesMultiple auth track_ids
  print audio_features
