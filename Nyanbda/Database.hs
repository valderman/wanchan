{-# LANGUAGE OverloadedStrings, TypeOperators, DeriveGeneric, FlexibleInstances, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nyanbda.Database
  ( Series (..), SeenEpisode (..), Resolution (..)
  , initialize
  , filterSeen, addSeen
  , addWatch, removeWatch, allWatched
  ) where
import Database.Selda
import Database.Selda.Backend
import Database.Selda.Generic
import Nyanbda.Types as Episode (Episode (..), Resolution (..))
import Data.Text (pack, unpack)

instance SqlType Resolution where
  mkLit Unknown = LCustom $ LInt 0
  mkLit SD480   = LCustom $ LInt 480
  mkLit HD720   = LCustom $ LInt 720
  mkLit HD1080  = LCustom $ LInt 1080
  sqlType _ = TInt
  fromSql (SqlInt 0)    = Unknown
  fromSql (SqlInt 480)  = SD480
  fromSql (SqlInt 720)  = HD720
  fromSql (SqlInt 1080) = HD1080
  fromSql x             = error $ "non-resolution resolution: " ++ show x
  defaultValue = mkLit Unknown

instance SqlType String where
  mkLit = LCustom . LText . pack
  sqlType _ = TText
  fromSql = unpack . fromSql
  defaultValue = mkLit ""

data Series = Series
  { seriesName :: String
  , seriesSeason :: Int
  , seriesGroup :: String
  , seriesResolution :: Resolution
  } deriving (Eq, Ord, Generic)

data SeenEpisode = SeenEpisode
  { seenName :: String
  , seenSeason :: Maybe Int
  , seenEpisode :: Maybe Int
  } deriving Generic

-- | The table of all seen episodes.
seenTable :: GenTable SeenEpisode
seenTable = genTable "seen" []

-- | The table of all watched series.
watchTable :: GenTable Series
watchTable = genTable "watched" []

-- | Initialize the database if it isn't already initialized.
initialize :: SeldaM ()
initialize = do
  tryCreateTable (gen seenTable)
  tryCreateTable (gen watchTable)

-- | Remove all episodes that already exist in the seen episodes list.
filterSeen :: [Episode] -> SeldaM [Episode]
filterSeen eps = fmap (map fromRel) . query $ do
    ep <- selectValues (map toRel eps)
    (seen :*: _) <- leftJoin (sameAs ep) (select (gen seenTable))
    restrict (isNull seen)
    return ep
  where
    sameAs ep (name :*: ssn :*: num) =
      third ep .== name .&&
      ((isNull ssn .&& isNull (fourth ep)) .|| fourth ep .== ssn) .&&
      ((isNull num .&& isNull (fifth ep)) .|| fifth ep .== num)

-- | Mark the given episodes as seen.
addSeen :: [Episode] -> SeldaM ()
addSeen = insertGen_ seenTable . map toSeenEpisode
  where
    toSeenEpisode ep = SeenEpisode
      { seenName = Episode.seriesName ep
      , seenSeason = seasonNumber ep
      , seenEpisode = episodeNumber ep
      }

-- | Add zero or more series to the watch list.
addWatch :: [Series] -> SeldaM ()
addWatch series = do
  mapM_ removeWatch series
  insertGen_ watchTable series

-- | Remove the give series from the watch list.
removeWatch :: Series -> SeldaM ()
removeWatch s = do
  deleteFrom_ (gen watchTable) $ \(n :*: ssn :*: g :*: r) ->
    n .== literal (Nyanbda.Database.seriesName s) .&&
    ssn .== literal (seriesSeason s) .&&
    g .== literal (seriesGroup s) .&&
    r .== literal (seriesResolution s)

-- | Get all series currently on the watch list.
allWatched :: SeldaM [Series]
allWatched = map fromRel <$> query (select (gen watchTable))
