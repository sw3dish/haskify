module Secrets where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified System.Environment as E
import qualified System.IO.Unsafe as U

--User specific information:
--This needs to be replaced with values obtained from developer.spotify.com
--in order to run test that make calls to the api
testClientId, testClientSecret :: B.ByteString
testClientId = C.pack (U.unsafePerformIO (E.getEnv "clientID"))
testClientSecret = C.pack (U.unsafePerformIO (E.getEnv "clientSecret"))
