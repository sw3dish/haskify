{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  testRequestToken
  testGetAlbum
  testGetAudioFeatures

testRequestToken = requestToken testClientId testClientSecret >>= print

testGetAlbum = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP" :: String
  auth <- fromJust <$> requestToken testClientId testClientSecret
  album <- getAlbum auth album_id
  print album

testGetAudioFeatures = do
  let track_id = "1ZLfI1KqHS2JFP7lKsC8bl" :: String
  auth <- fromJust <$> requestToken testClientId testClientSecret
  audio_features <- getAudioFeatures auth track_id
  print audio_features
