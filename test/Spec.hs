{-# LANGUAGE OverloadedStrings #-}

import Haskify
import Types

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
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
    runTest "testGetFeaturedPlaylists" testGetFeaturedPlaylists
    runTest "testGetNewReleases" testGetNewReleases
    runTest "testGetCategoryMultiple" testGetCategoryMultiple
    runTest "testGetCategorySingle" testGetCategorySingle
    runTest "testGetCategoryPlaylists" testGetCategoryPlaylists
    runTest "testGetTrackSingle" testGetTrackSingle
    runTest "testGetTrackMultiple" testGetTrackMultiple
    runTest "testGetPagingNext" testGetPagingNext
    runTest "testGetPagingNext_FeaturedPlaylists" testGetPagingNext_FeaturedPlaylists
    runTest "testGetPagingNext_NewReleases" testGetPagingNext_NewReleases
    runTest "testGetPagingNext_CategoryMultiple" testGetPagingNext_CategoryMultiple
    runTest "testSearchAlbum" testSearchAlbums
    runTest "testSearchAll" testSearchAll) (Token undefined undefined)
  return ()
    where runTest name test = do
            liftIO $ putStr name
            test  <|> liftIO (putStrLn " fail")
            liftIO $ putStrLn " pass"

testRequestToken :: HaskifyAction ()
testRequestToken = do
  requestToken testClientId testClientSecret
  get
  return ()

testGetAlbumSingle :: HaskifyAction ()
testGetAlbumSingle = do
  let albumId = "3EwfQtjvyRAXsPWAKO5FDP"
  let optionalParameters = [("market", "US")]
  requestToken testClientId testClientSecret
  getAlbumSingle albumId optionalParameters
  return ()

testGetAlbumMultiple :: HaskifyAction ()
testGetAlbumMultiple = do
  let albumIds = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"]
  let optionalParameters = [("market", "US")]
  requestToken testClientId testClientSecret
  getAlbumMultiple albumIds optionalParameters
  return ()

testGetAlbumTracks :: HaskifyAction ()
testGetAlbumTracks = do
  let albumId = "6084R9tVaGpB9yefy7ObuQ"
  let optionalParameters = [("limit", "5"), ("offset", "1"), ("market", "US")]
  requestToken testClientId testClientSecret
  getAlbumTracks albumId optionalParameters
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
  let optionalParameters = [("album_type", "album"), ("market", "US"), ("limit", "5"), ("offset", "0")]
  requestToken testClientId testClientSecret
  getArtistAlbums artistId optionalParameters
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

testGetFeaturedPlaylists :: HaskifyAction ()
testGetFeaturedPlaylists = do
  let optionalParameters = [("locale", "en_US"), ("country", "US"), ("timestamp", "2014-10-23T09:00:00"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  getFeaturedPlaylists optionalParameters
  return ()

testGetPagingNext_FeaturedPlaylists :: HaskifyAction ()
testGetPagingNext_FeaturedPlaylists = do
  let optionalParameters = [("locale", "en_US"), ("country", "US"), ("timestamp", "2014-10-23T09:00:00"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  (FeaturedPlaylistsResponse (_, testPage)) <- getFeaturedPlaylists optionalParameters
  nextPage <- getPagingNext testPage
  return ()

testGetNewReleases :: HaskifyAction ()
testGetNewReleases = do
  let optionalParameters = [("country", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  getNewReleases optionalParameters
  return ()

testGetPagingNext_NewReleases :: HaskifyAction ()
testGetPagingNext_NewReleases = do
  let optionalParameters = [("country", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  (NewReleasesResponse (_, testPage)) <- getNewReleases optionalParameters
  nextPage <- getPagingNext testPage
  return ()

testGetCategoryMultiple :: HaskifyAction ()
testGetCategoryMultiple = do
  let optionalParameters = [("locale", "en_US"), ("country", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  getCategoryMultiple optionalParameters
  return ()

testGetPagingNext_CategoryMultiple  :: HaskifyAction ()
testGetPagingNext_CategoryMultiple = do
  let optionalParameters = [("locale", "en_US"), ("country", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  (CategoriesResponse testPage) <- getCategoryMultiple optionalParameters
  --liftIO $ print testPage
  nextPage <- getPagingNext testPage
  return ()

testGetCategorySingle :: HaskifyAction ()
testGetCategorySingle = do
  let categoryId = "party"
  let optionalParameters = [("locale", "en_US"), ("country", "US")]
  requestToken testClientId testClientSecret
  getCategorySingle categoryId optionalParameters
  return ()

testGetCategoryPlaylists :: HaskifyAction ()
testGetCategoryPlaylists = do
  let categoryId = "party"
  let optionalParameters = [("country", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  getCategoryPlaylists categoryId optionalParameters
  return ()

testGetTrackSingle :: HaskifyAction ()
testGetTrackSingle = do
  let trackId = "1ZLfI1KqHS2JFP7lKsC8bl"
  let optionalParameters = [("market", "US")]
  requestToken testClientId testClientSecret
  getTrackSingle trackId optionalParameters
  return ()

testGetTrackMultiple :: HaskifyAction ()
testGetTrackMultiple = do
  let trackIds = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"]
  let optionalParameters = [("market", "US")]
  requestToken testClientId testClientSecret
  getTrackMultiple trackIds optionalParameters
  return ()

testSearchAlbums :: HaskifyAction ()
testSearchAlbums = do
  let optionalParameters = [("market", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  (SearchResponse (_,y,x))  <- search [SearchTypeAlbum] "test" optionalParameters
  _ <- haskifyLiftMaybe y
  return ()

testSearchAll :: HaskifyAction ()
testSearchAll = do
  let optionalParameters = [("market", "US"), ("limit", "5"), ("offset", "1")]
  requestToken testClientId testClientSecret
  (SearchResponse (x,y,z)) <- search [SearchTypeTrack, SearchTypeAlbum, SearchTypeArtist] "test" optionalParameters
  _ <- haskifyLiftMaybe x
  _ <- haskifyLiftMaybe y
  _ <- haskifyLiftMaybe z
  return ()

testGetPagingNext :: HaskifyAction ()
testGetPagingNext = do
  requestToken testClientId testClientSecret
  -- this album should have enough tracks to trigger a paging
  testPage <- album_tracks <$> getAlbumSingle "1lgOEjXcAJGWEQO1q4akqu" []
  nextPage <- getPagingNext testPage
  return ()
