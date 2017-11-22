{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  runMaybeT $ do
    testRequestToken
    testGetAlbumSingle
    testGetAlbumMultiple
    testGetAudioFeaturesSingle
    testGetAudioFeaturesMultiple
    testGetRecentReleases
  return ()

testRequestToken = requestToken testClientId testClientSecret >>= (liftIO . print)

testGetAlbumSingle = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  auth <- requestToken testClientId testClientSecret
  album <- getAlbumSingle auth album_id
  liftIO $ print album

testGetAlbumMultiple = do
  let album_ids = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"] :: [String]
  auth <- requestToken testClientId testClientSecret
  albums <- getAlbumMultiple auth album_ids
  liftIO $ print albums

testGetAudioFeaturesSingle = do
  let track_id = "1ZLfI1KqHS2JFP7lKsC8bl" :: String
  auth <- requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesSingle auth track_id
  liftIO $ print audio_features

testGetAudioFeaturesMultiple = do
  let track_ids = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"] :: [String]
  auth <- requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesMultiple auth track_ids
  liftIO $ print audio_features

testGetRecentReleases = do
  auth  <- requestToken testClientId testClientSecret
  album <- getNewReleases auth
  liftIO $ print album
