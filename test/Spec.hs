{-# LANGUAGE OverloadedStrings #-}

import Haskify

import Secrets (testClientId, testClientSecret)

main :: IO ()
main = do
  testRequestToken

testRequestToken = requestToken testClientId testClientSecret >>= print
