{-# LANGUAGE OverloadedStrings #-}

import Haskify

import Secrets

main :: IO ()
main = do
  testRequestToken

testRequestToken = requestToken testClientId testClientSecret >>= print
