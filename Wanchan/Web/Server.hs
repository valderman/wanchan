{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Wanchan.Web.Server
  (find, addSeries, delSeries, getWatchList, triggerCheck
  , updateTrigger, triggerUpdate) where
import Haste.App
import Control.Shell hiding (Result)
import Wanchan.Config
import Wanchan.Database
import Wanchan.Sources
import Wanchan.Types
import Wanchan.Web.Config
import Wanchan.Web.API
import Data.List (sort, group)
import Data.Maybe (isJust)
import Database.Selda.Backend (runSeldaT)
import Control.Concurrent
import System.IO.Unsafe

-- | Perform an episode search using the given config and search term.
search :: Config -> String -> Shell [Episode]
search cfg str = do
    concat <$> mapM (\h -> h str) handlers
  where
    handlers = map srcHandler $ cfgSources cfg

-- | Search for all series given the matching string.
find :: Import (String -> Auth -> Server [Series])
find = remote $ \s -> withToken $ liftIO $ do
  cfg <- snd <$> getWebConfig
  mkSeries <$> shell_ (search cfg s)

mkSeries :: [Episode] -> [Series]
mkSeries eps = snub
  [ Series
    { Wanchan.Database.seriesName = Wanchan.Types.seriesName ep
    , seriesGroup = group
    , seriesResolution = res
    , seriesSeason = maybe 1 id (seasonNumber ep)
    }
  | ep <- eps
  , isJust (releaseGroup ep)
  , let Just group = releaseGroup ep
  , let res = maybe Unknown id (resolution ep)
  ]
  where
    snub = map head . group . sort

-- | Add a series to the watch list.
addSeries :: Import (Series -> Auth -> Server [Series])
addSeries = remote $ \s -> withToken $ liftIO $ do
  db <- fst <$> getWebConfig
  flip runSeldaT db $ do
    addWatch [s]
    allWatched

-- | Remove a series from the watch list.
delSeries :: Import (Series -> Auth -> Server [Series])
delSeries = remote $ \s -> withToken $ liftIO $ do
  db <- fst <$> getWebConfig
  flip runSeldaT db $ do
    removeWatch s
    allWatched

-- | Get all series on the watch list.
getWatchList :: Import (Auth -> Server [Series])
getWatchList = remote $ withToken $ liftIO $ do
  db <- fst <$> getWebConfig
  runSeldaT allWatched db

-- | Verify a session "token", which is currently just a pair of username and
--   password.
withToken :: Server a -> Auth -> Server a
withToken m auth = do
  cfg <- snd <$> liftIO getWebConfig
  if cfgWebUser cfg == authUser auth && cfgWebPassword cfg == authPass auth
    then m
    else error "bad username or password"

-- | Trigger a series update.
triggerCheck :: Import (Auth -> Server ())
triggerCheck = remote $ withToken $ liftIO triggerUpdate

{-# NOINLINE updateTrigger #-}
-- | Write to this MVar to trigger an update. Only consumed by web daemon.
updateTrigger :: MVar ()
updateTrigger = unsafePerformIO $ newEmptyMVar

-- | Trigger an episode update from the web daemon.
triggerUpdate :: MonadIO m => m ()
triggerUpdate = liftIO $ putMVar updateTrigger ()
