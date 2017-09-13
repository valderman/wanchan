{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Nyanbda.Web.Server (find, addSeries, delSeries, getWatchList) where
import Haste.App
import Control.Shell hiding (Result)
import Nyanbda.Config
import Nyanbda.Database
import Nyanbda.Filtering
import Nyanbda.Sources
import Nyanbda.Types
import Nyanbda.Web.Config
import Nyanbda.Web.API
import Data.List (sort, group)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Database.Selda hiding (Result)
import Database.Selda.SQLite
import Database.Selda.Backend (runSeldaT)

-- | Perform an episode search using the given config and search term.
search :: Config -> String -> Shell [Episode]
search cfg str = do
    concat <$> mapM (\h -> h str) handlers
  where
    handlers = map srcHandler $ cfgSources cfg

-- | Search for all series given the matching string.
find :: Import (String -> Server [Series])
find = remote $ \s -> liftIO $ do
  cfg <- snd <$> getWebConfig
  mkSeries <$> shell_ (search cfg s)

mkSeries :: [Episode] -> [Series]
mkSeries eps = snub
  [ Series
    { Nyanbda.Database.seriesName = Nyanbda.Types.seriesName ep
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
addSeries :: Import (Series -> Server [Series])
addSeries = remote $ \s -> liftIO $ do
  db <- fst <$> getWebConfig
  flip runSeldaT db $ do
    addWatch [s]
    allWatched

-- | Remove a series from the watch list.
delSeries :: Import (Series -> Server [Series])
delSeries = remote $ \s -> liftIO $ do
  db <- fst <$> getWebConfig
  flip runSeldaT db $ do
    removeWatch s
    allWatched

-- | Get all series on the watch list.
getWatchList :: Import (Server [Series])
getWatchList = remote $ liftIO $ do
  db <- fst <$> getWebConfig
  runSeldaT allWatched db
