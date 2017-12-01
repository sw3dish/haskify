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
    runTest "testGetAlbumTracks" testGetAlbumTracks
    runTest "testGetArtistSingle" testGetArtistSingle
    runTest "testGetArtistMultiple" testGetArtistMultiple
    runTest "testGetArtistAlbums" testGetArtistAlbums
    runTest "testGetArtistTopTracks" testGetArtistTopTracks
    runTest "testGetArtistRelatedArtists" testGetArtistRelatedArtists
    runTest "testGetAudioFeaturesSingle" testGetAudioFeaturesSingle
    runTest "testGetAudioFeaturesMultiple" testGetAudioFeaturesMultiple
    runTest "testGetRecentReleases" testGetRecentReleases
    --runTest "printF" printF
    ) (Token undefined undefined)
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
  let albumId = "3EwfQtjvyRAXsPWAKO5FDP"
  requestToken testClientId testClientSecret
  getAlbumSingle albumId
  return ()

testGetAlbumMultiple :: HaskifyAction ()
testGetAlbumMultiple = do
  let albumIds = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"]
  requestToken testClientId testClientSecret
  getAlbumMultiple albumIds
  return ()

testGetAlbumTracks :: HaskifyAction ()
testGetAlbumTracks = do
  let albumId = "6084R9tVaGpB9yefy7ObuQ"
  requestToken testClientId testClientSecret
  getAlbumTracks albumId
  return ()

testGetArtistSingle :: HaskifyAction ()
testGetArtistSingle = do
  let artistId = "4DMSJzGjw2SMkKAT5EEE5u"
  requestToken testClientId testClientSecret
  getArtistSingle artistId
  return ()

testGetArtistMultiple :: HaskifyAction ()
testGetArtistMultiple = do
  let artistIds = ["4DMSJzGjw2SMkKAT5EEE5u", "6P7H3ai06vU1sGvdpBwDmE", "3WaJSfKnzc65VDgmj2zU8B"]
  requestToken testClientId testClientSecret
  getArtistMultiple artistIds
  return ()

testGetArtistAlbums :: HaskifyAction ()
testGetArtistAlbums = do
  let artistId = "4DMSJzGjw2SMkKAT5EEE5u"
  requestToken testClientId testClientSecret
  getArtistAlbums artistId
  return ()

testGetArtistTopTracks :: HaskifyAction ()
testGetArtistTopTracks = do
  let artistId = "4DMSJzGjw2SMkKAT5EEE5u"
  let country = "US"
  requestToken testClientId testClientSecret
  getArtistTopTracks artistId country
  return ()

testGetArtistRelatedArtists :: HaskifyAction ()
testGetArtistRelatedArtists = do
  let artistId = "4DMSJzGjw2SMkKAT5EEE5u"
  requestToken testClientId testClientSecret
  getArtistRelatedArtists artistId
  return ()

testGetAudioAnalysis :: HaskifyAction ()
testGetAudioAnalysis = do
  let trackId = "1ZLfI1KqHS2JFP7lKsC8bl"
  requestToken testClientId testClientSecret
  getAudioAnalysis trackId
  return ()

testGetAudioFeaturesSingle :: HaskifyAction ()
testGetAudioFeaturesSingle = do
  let trackId = "1ZLfI1KqHS2JFP7lKsC8bl"
  requestToken testClientId testClientSecret
  getAudioFeaturesSingle trackId
  return ()

testGetAudioFeaturesMultiple :: HaskifyAction ()
testGetAudioFeaturesMultiple = do
  let trackIds = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"]
  requestToken testClientId testClientSecret
  getAudioFeaturesMultiple trackIds
  return ()

testGetRecentReleases :: HaskifyAction ()
testGetRecentReleases = do
  requestToken testClientId testClientSecret
  getNewReleases
  return ()

printF :: HaskifyAction ()
printF = do
  let artistId = "4DMSJzGjw2SMkKAT5EEE5u"
  requestToken testClientId testClientSecret
  artist <- getArtistSingle artistId
  liftIO $ print artist
  return ()
