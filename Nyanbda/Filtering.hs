{-# LANGUAGE RecordWildCards #-}
module Nyanbda.Filtering (filterEpisodes) where
import Data.Maybe
import Nyanbda.Config
import Nyanbda.Types

-- | Filter a list of episodes based on the given config.
--   TODO: latest episode filtering
filterEpisodes :: Config -> [Episode] -> [Episode]
filterEpisodes (Config {..}) =
    filter p
  where
    p x = all (\p' -> p' x) activeFilters
    activeFilters = catMaybes $
      [ maybeFilter seasons     cfgSeasons
      , maybeFilter episodes    cfgEpisodes
      , maybeFilter exts        cfgExtensions
      , maybeFilter resolutions cfgResolutions
      , maybeFilter groups      cfgGroups
      ]

    seasons (Episode {..})
      | Just s <- seasonNumber  = s `elem` cfgSeasons
      | otherwise               = False
    episodes (Episode {..})
      | Just e <- episodeNumber = e `elem` cfgEpisodes
      | otherwise               = False
    exts (Episode {..})
      | Just e <- fileExtension = e `elem` cfgExtensions
      | otherwise               = False
    groups (Episode {..})
      | Just g <- releaseGroup  = g `elem` cfgGroups
      | otherwise               = False
    resolutions (Episode {..})  = resolution `elem` cfgResolutions

    maybeFilter f conf
      | null conf = Nothing
      | otherwise = Just f
