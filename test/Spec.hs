{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as B

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  testRequestToken
  testGetAlbum

testRequestToken = requestToken testClientId testClientSecret >>= print

testGetAlbum = do
  let album_id = "3EwfQtjvyRAXsPWAKO5FDP"
  auth <- fromJust <$> requestToken testClientId testClientSecret
  album <- getAlbum auth album_id
  print album
