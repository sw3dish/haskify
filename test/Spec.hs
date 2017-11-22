{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Types
import Data.Maybe (fromJust)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
--  testRequestToken
--  testGetAlbumSingle
--  testGetAlbumMultiple
--  testGetAudioFeaturesSingle
--  testGetAudioFeaturesMultiple
  testGetPagingNext
--  testGetRecentReleases

testRequestToken = requestToken testClientId testClientSecret >>= print

testGetAlbumSingle = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  auth <- requestToken testClientId testClientSecret
  album <- join <$> (sequence $ getAlbumSingle <$> auth <*> (pure album_id))
  print album

testGetAlbumMultiple = do
  let album_ids = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"] :: [String]
  auth <- fromJust <$> requestToken testClientId testClientSecret
  albums <- getAlbumMultiple auth album_ids
  print albums

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

testGetPagingNext = do
  auth <- requestToken testClientId testClientSecret
  testPage <- join <$> (sequence $ getNewReleases <$> auth )
  print testPage
  nextPage <- sequence $ (getPagingNext <$> auth <*> ( newreleases_albums <$> testPage))
  print nextPage

testGetRecentReleases = do
  auth <- requestToken testClientId testClientSecret
  album <- join <$> (sequence $ getNewReleases <$> auth )
  print album
