{-# LANGUAGE OverloadedStrings #-}

import Haskify

main :: IO ()
main = do
  testRequestToken

--User specific information:
--This needs to be replaced with values obtained from developer.spotify.com
--in order to run test that make calls to the api
testClientId = error "must define test_client_id"
testClientSecret = error "must define test_client_secret"

testRequestToken = requestToken testClientId testClientSecret >>= print
