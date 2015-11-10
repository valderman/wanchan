{-# LANGUAGE RecordWildCards #-}
module Nyanbda.Filtering (filterEpisodes) where
import Data.List
import Data.Maybe
import Nyanbda.Config
import Nyanbda.Types

-- | Filter a list of episodes based on the given config.
--   TODO: latest episode filtering
--   TODO: select episodes based on consistency; i.e. prefer to get the same
--         release group, resolution, etc. for all episodes
filterEpisodes :: Config -> [Episode] -> [Episode]
filterEpisodes (Config {..}) allEps
    | cfgAllowDupes = eps
    | otherwise     = chooseEpisodes eps
  where
    eps = filter p allEps
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

-- | Choose one instance of each season/episode number pair.
chooseEpisodes :: [Episode] -> [Episode]
chooseEpisodes = map (head . sortEpisodes) . groupEpisodes

-- | Sort a list of episodes based on their "desirability".
sortEpisodes :: [Episode] -> [Episode]
sortEpisodes = sortBy (compareAll criteria)
  where
    criteria = [ \a b -> resolution a `compare` resolution b
               , compBy releaseGroup
               , compBy fileExtension
               ]

compareAll :: [a -> a -> Ordering] -> a -> a -> Ordering
compareAll (cmp : xs) a b =
  case a `cmp` b of
    EQ  -> compareAll xs a b
    ord -> ord
compareAll _ _ _ =
  EQ

-- | Group episodes by season and episode number.
groupEpisodes :: [Episode] -> [[Episode]]
groupEpisodes = groupBy sameEp . sortBy orderEp
  where
    orderEp = compareAll [ \a b -> seriesName a `compare` seriesName b
                         , compBy seasonNumber
                         , compBy episodeNumber
                         ]
    sameEp a b = and [ seriesName a == seriesName b
                     , eqBy seasonNumber a b
                     , eqBy episodeNumber a b
                     ]

-- | Compare two episodes on the given attribute. A missing attribute is
--   considered "less" than a present one.
compBy :: Ord a => (Episode -> Maybe a) -> Episode -> Episode -> Ordering
compBy f a b =
  case (f a, f b) of
    (Just x, Just y) -> x `compare` y
    (Just _, _)      -> GT
    _                -> LT

-- | Compare two episodes for equality on the given attribute.
eqBy :: Eq a => (Episode -> Maybe a) -> Episode -> Episode -> Bool
eqBy f a b = maybe False id $ do
  x <- f a
  y <- f b
  pure (x == y)
