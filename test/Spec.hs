{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Data.Maybe (fromJust)
import Control.Monad
import qualified Data.ByteString.Lazy as B

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  testRequestToken
  testGetAlbum

testRequestToken = requestToken testClientId testClientSecret >>= print

testGetAlbum = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  auth <- requestToken testClientId testClientSecret
  album <- join <$> (sequence $ getAlbum <$> auth <*> (pure album_id))
  print album
