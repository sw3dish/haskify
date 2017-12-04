module Secrets where 

import qualified Data.ByteString as B

--User specific information:
--This needs to be replaced with values obtained from developer.spotify.com
--in order to run test that make calls to the api
testClientId, testClientSecret :: B.ByteString
testClientId = undefined
testClientSecret = undefined
