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

main :: IO ()
main = do
  runMaybeT $ runStateT (do 
    testRequestToken             <|> (lift . lift $ print "fail")
    testGetAlbumSingle           <|> (lift . lift $ print "fail")
    testGetAlbumMultiple         <|> (lift . lift $ print "fail")
    testGetAudioFeaturesSingle   <|> (lift . lift $ print "fail")
    testGetAudioFeaturesMultiple <|> (lift . lift $ print "fail")
    testGetRecentReleases        <|> (lift . lift $ print "fail")
    ) (Token undefined undefined)
  return ()

testRequestToken :: HaskifyAction ()
testRequestToken = do
  requestToken testClientId testClientSecret 
  tok <- get
  lift . lift $ print tok

testGetAlbumSingle = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  requestToken testClientId testClientSecret
  album <- getAlbumSingle album_id
  lift . lift $ print album

testGetAlbumMultiple = do
  let album_ids = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"]
  requestToken testClientId testClientSecret
  albums <- getAlbumMultiple album_ids
  lift . lift $ print albums

testGetAudioFeaturesSingle = do
  let track_id = "1ZLfI1KqHS2JFP7lKsC8bl" :: String
  requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesSingle track_id
  lift . lift $ print audio_features

testGetAudioFeaturesMultiple = do
  let track_ids = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"] :: [String]
  requestToken testClientId testClientSecret
  audio_features <- getAudioFeaturesMultiple track_ids
  lift . lift $ print audio_features

testGetRecentReleases = do
  requestToken testClientId testClientSecret
  album <- getNewReleases 
  lift . lift $ print album
