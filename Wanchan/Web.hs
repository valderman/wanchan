{-# LANGUAGE OverloadedStrings, StaticPointers, FlexibleInstances, CPP, TypeFamilies #-}
module Wanchan.Web where
import Control.Monad
import Haste.App
import Haste.DOM
import Haste.Events
import Wanchan.Web.API
import System.IO.Unsafe
import Wanchan.Web.ClientAuth
import Wanchan.Web.Dialog
import Haste.Foreign
import System.IO.Unsafe

#ifndef __HASTE__
import Wanchan.Web.Server
#else
find = undefined
addSeries = undefined
delSeries = undefined
getWatchList = undefined
#endif

data ContextMenuEvent = ContextMenu

instance Event ContextMenuEvent where
  type EventData ContextMenuEvent = MouseData
  eventName ContextMenu = "contextmenu"
  eventData _ x = eventData MouseUp x

encodeURI' :: String -> IO String
encodeURI' = ffi "encodeURI"

encodeURI :: String -> String
encodeURI = unsafePerformIO . encodeURI'

toGlobal :: Elem -> (Int, Int) -> IO (Int, Int)
toGlobal = ffi "(function(e, c) {var br = e.getBoundingClientRect(); return [c[0]+br.left, c[1]+br.top];})"

searchBar, searchBtn, searchResults, watchList :: Elem
[searchBar, searchBtn, searchResults, watchList] =
  unsafePerformIO $ withElems
    [ "searchbar"
    , "searchbtn"
    , "results"
    , "watchlist"
    ] return

doFind :: RemotePtr (String -> Auth -> Server [Series])
doFind = static find

doAddSeries :: RemotePtr (Series -> Auth -> Server [Series])
doAddSeries = static addSeries

doDelSeries :: RemotePtr (Series -> Auth -> Server [Series])
doDelSeries = static delSeries

doGetWatched :: RemotePtr (Auth -> Server [Series])
doGetWatched = static getWatchList

webMain :: IO ()
webMain = runApp [start (Proxy :: Proxy Server)] $ void $ do
  searchBar `onEvent` KeyPress $ \13 -> trySearch searchResults searchBar
  searchBtn `onEvent` Click $ const $ trySearch searchResults searchBar
  handshake 0

handshake :: Int -> Client ()
handshake retries = do
  flip catchError (\_ -> authDialog (handshake (retries+1))) $ do
    fork $ trySearch searchResults searchBar
    withAuth (dispatch doGetWatched) >>= mapM mkDelButton >>= setChildren watchList

-- | Get the search string from the given search bar, try the search on the
--   server, and fill the given list element with the results.
trySearch :: Elem -> Elem -> Client ()
trySearch results searchbar = do
  setAttr searchbar "disabled" "true"
  populateResults results =<< withAuth (dispatch doFind) =<< getProp searchbar "value"
  unsetAttr searchbar "disabled"
  focus searchbar

populateResults :: Elem -> [Series] -> Client ()
populateResults list rs = do
    setChildren list =<< mapM mkResultDialogButton rs
  where
    mkResultDialogButton s = do
      let name = seriesName s
      dlg <- createChoiceDialog name
        [ ("Add to watch list", Run $ addToWatchList s)
        , ("Search AniList", Open $ searchAniList name)
        , ("Search ANN", Open $ searchANN name)
        ]
      newSeriesListItem s (const $ addToWatchList s, showDialogAt dlg)

    searchAniList s = "https://anilist.co/search?type=all&q=" ++ encodeURI s
    searchANN s = "https://www.animenewsnetwork.com/encyclopedia/search/name?q=" ++ encodeURI s

-- | Create a new list item element from the given series, which performs
--   the given computation when clicked.
newSeriesListItem :: Series -> ((Int, Int) -> Client (), (Int, Int) -> Client ()) -> Client Elem
newSeriesListItem series (l, r) = do
  link <- newElem "a" `with`
    [ "innerHTML" =: show series
    , "href" =: "javascript:void(0);"
    ]
  void $ link `onEvent` Click $ \d ->
    when (mouseButton d == Just MouseLeft) $ do
      (liftIO (toGlobal link (mouseCoords d)) >>= l)
  void $ link `onEvent` ContextMenu $ \d ->
    preventDefault >> liftIO (toGlobal link (mouseCoords d)) >>= r
  newElem "li" `with` [children [link], "className" =: "resultrow"]

-- | Create a new link to remove a series from the watch list.
mkDelButton :: Series -> Client Elem
mkDelButton series =
  newSeriesListItem series (const $ delFromWatchList series, const $ pure ())

-- | Add the given series to the watch list.
addToWatchList :: Series -> Client ()
addToWatchList series = do
  watched <- withAuth (dispatch doAddSeries) series
  ws <- mapM mkDelButton watched
  setChildren watchList ws

-- | Remove the given series from the watch list.
delFromWatchList :: Series -> Client ()
delFromWatchList series = do
  watched <- withAuth (dispatch doDelSeries) series
  ws <- mapM mkDelButton watched
  setChildren watchList ws
