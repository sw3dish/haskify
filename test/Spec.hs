{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Types
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative

import Secrets (testClientId, testClientSecret)

-- These test function are very basic and only verify that the API calls are completed succesfully.
-- No effort is made to verify that the result is expected/valid.
main :: IO ()
main = do
  runMaybeT $ runStateT (do 
    runTest "testRequestToken" testRequestToken
    runTest "testGetAlbumSingle" testGetAlbumSingle
    runTest "testGetAlbumMultiple" testGetAlbumMultiple
    runTest "testGetAudioFeaturesSingle" testGetAudioFeaturesSingle
    runTest "testGetAudioFeaturesMultiple" testGetAudioFeaturesMultiple
    runTest "testGetRecentReleases" testGetRecentReleases
    runTest "testSearchAlbum" testSearchAlbums
    runTest "testSearchAll" testSearchAll) (Token undefined undefined)
  return ()
    where runTest name test = do
            liftIO $ putStr name
            test  <|> (liftIO $ putStrLn " fail")
            liftIO $ putStrLn " pass"

testRequestToken :: HaskifyAction ()
testRequestToken = do
  requestToken testClientId testClientSecret 
  get
  return ()

testGetAlbumSingle :: HaskifyAction ()
testGetAlbumSingle = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  requestToken testClientId testClientSecret
  getAlbumSingle album_id
  return ()

testGetAlbumMultiple :: HaskifyAction ()
testGetAlbumMultiple = do
  let album_ids = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"]
  requestToken testClientId testClientSecret
  getAlbumMultiple album_ids
  return ()

testGetAudioFeaturesSingle :: HaskifyAction ()
testGetAudioFeaturesSingle = do
  let track_id = "1ZLfI1KqHS2JFP7lKsC8bl" :: String
  requestToken testClientId testClientSecret
  getAudioFeaturesSingle track_id
  return ()

testGetAudioFeaturesMultiple :: HaskifyAction ()
testGetAudioFeaturesMultiple = do
  let track_ids = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"] :: [String]
  requestToken testClientId testClientSecret
  getAudioFeaturesMultiple track_ids
  return ()

testGetRecentReleases :: HaskifyAction ()
testGetRecentReleases = do
  requestToken testClientId testClientSecret
  getNewReleases
  return ()

testSearchAlbums :: HaskifyAction ()
testSearchAlbums = do
  requestToken testClientId testClientSecret
  (SearchResponse (_,y,x))  <- search [SearchTypeAlbum] "test"
  _ <- haskifyLiftMaybe y
  return ()

testSearchAll :: HaskifyAction ()
testSearchAll = do
  requestToken testClientId testClientSecret
  (SearchResponse (x,y,z)) <- search [SearchTypeTrack, SearchTypeAlbum, SearchTypeArtist] "test"
  _ <- haskifyLiftMaybe x
  _ <- haskifyLiftMaybe y
  _ <- haskifyLiftMaybe z
  return ()
