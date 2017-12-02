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
  requestToken testClientId testClientSecret
  getAlbumSingle albumId
  return ()

testGetAlbumMultiple :: HaskifyAction ()
testGetAlbumMultiple = do
  let albumIds = ["6084R9tVaGpB9yefy7ObuQ", "5Dbax7G8SWrP9xyzkOvy2F", "2DuGRzpsUNe6jzGpCniZYR"]
  requestToken testClientId testClientSecret
  getAlbumMultiple albumIds
  return ()

testGetAlbumTracks :: HaskifyAction ()
testGetAlbumTracks = do
  let albumId = "6084R9tVaGpB9yefy7ObuQ"
  requestToken testClientId testClientSecret
  getAlbumTracks albumId
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
  requestToken testClientId testClientSecret
  getArtistAlbums artistId
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
  requestToken testClientId testClientSecret
  getFeaturedPlaylists
  return ()

testGetPagingNext_FeaturedPlaylists :: HaskifyAction ()
testGetPagingNext_FeaturedPlaylists = do
  requestToken testClientId testClientSecret
  (FeaturedPlaylistsResponse (_, testPage)) <- getFeaturedPlaylists
  nextPage <- getPagingNext testPage
  return ()

testGetNewReleases :: HaskifyAction ()
testGetNewReleases = do
  requestToken testClientId testClientSecret
  getNewReleases
  return ()

testGetPagingNext_NewReleases :: HaskifyAction ()
testGetPagingNext_NewReleases = do
  requestToken testClientId testClientSecret
  (NewReleasesResponse (_, testPage)) <- getNewReleases
  nextPage <- getPagingNext testPage
  return ()

testGetCategoryMultiple :: HaskifyAction ()
testGetCategoryMultiple = do
  requestToken testClientId testClientSecret
  getCategoryMultiple
  return ()

testGetPagingNext_CategoryMultiple  :: HaskifyAction ()
testGetPagingNext_CategoryMultiple = do
  requestToken testClientId testClientSecret
  (CategoriesResponse testPage) <- getCategoryMultiple
  --liftIO $ print testPage
  nextPage <- getPagingNext testPage
  return ()

testGetCategorySingle :: HaskifyAction ()
testGetCategorySingle = do
  let categoryId = "party"
  requestToken testClientId testClientSecret
  getCategorySingle categoryId
  return ()

testGetCategoryPlaylists :: HaskifyAction ()
testGetCategoryPlaylists = do
  let categoryId = "party"
  requestToken testClientId testClientSecret
  getCategoryPlaylists categoryId
  return ()

testGetTrackSingle :: HaskifyAction ()
testGetTrackSingle = do
  let trackId = "1ZLfI1KqHS2JFP7lKsC8bl"
  requestToken testClientId testClientSecret
  getTrackSingle trackId
  return ()

testGetTrackMultiple :: HaskifyAction ()
testGetTrackMultiple = do
  let trackIds = ["1ZLfI1KqHS2JFP7lKsC8bl", "2MW0ofGJTi9RfoCMPsfGrJ", "2jz1bw1p0WQj0PDnVDP0uY"]
  requestToken testClientId testClientSecret
  getTrackMultiple trackIds
  return ()

testSearchAlbums :: HaskifyAction ()
testSearchAlbums = do
  requestToken testClientId testClientSecret
  (SearchResponse (_,y,x))  <- search [SearchTypeAlbum] "test"
  _ <- haskifyLiftMaybe y
  return ()

testSearchAll :: HaskifyAction ()
testSearchAll = do
  requestToken testClientId testClientSecret
  (SearchResponse (x,y,z)) <- search [SearchTypeTrack, SearchTypeAlbum, SearchTypeArtist] "test"
  _ <- haskifyLiftMaybe x
  _ <- haskifyLiftMaybe y
  _ <- haskifyLiftMaybe z
  return ()

testGetPagingNext :: HaskifyAction ()
testGetPagingNext = do
  requestToken testClientId testClientSecret
  -- this album should have enough tracks to trigger a paging
  testPage <- album_tracks <$> getAlbumSingle "1lgOEjXcAJGWEQO1q4akqu"
  nextPage <- getPagingNext testPage
  return ()
