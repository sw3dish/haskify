module Main where

import Haskify
import Types

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

import qualified Data.Text as T
import Data.List

import System.Environment
import qualified Data.ByteString.Char8 as B

import GHC.IO.Encoding

main :: IO ()
main = void . runMaybeT $ runStateT (do
  liftIO $ setLocaleEncoding utf8


  -- Obtain a client id and key from the user
  (clientId:clientSecret:_) <- liftIO $ map B.pack <$> getArgs

  -- Use this to obtain a token from the spotify server
  requestToken clientId clientSecret

  -- This token is stored in a State monad so it can be retrieved
  -- with a call to get. This is how the token is obtained by internal
  -- haskify functions but is not neccercary when using the library.
  get >>= (liftIO . print)

  -- A call to the api is made through a simple monadic action
  -- The result of the call is returned wrapped in our monad transformer
  -- Type but can be extracted with a bind
  (NewReleasesResponse (_,albumPaging)) <- getNewReleases []

  -- For his particular endpoint, the result is also wrapped in a Paging
  -- meaning the API only returns some smaller number items.
  -- To retrieve the full list of results, another function is called.
  albumList <- collectPaging albumPaging

  -- Having the full list of results, operations can be performed on the data
  let albumNames  = sort $ map albumsimplified_name albumList
  liftIO $ putStrLn "\n=======================\nNew Releases Album Names\n=======================\n"
  forM_ albumNames (liftIO . putStrLn . T.unpack)

  let artistNames = sort . nub . map artistsimplified_name . concatMap albumsimplified_artists $ albumList
  liftIO $ putStrLn "\n=======================\nNew Releases Artist Names\n=======================\n"
  forM_ artistNames (liftIO . putStrLn . T.unpack)

  ) (Token undefined undefined)
