{-# LANGUAGE RecordWildCards #-}
module Nyanbda.Filtering (filterEpisodes, filterSeen) where
import Data.List
import Data.Maybe
import Nyanbda.Config
import Nyanbda.Types

-- | Remove all episodes that were already seen. Does not preserve the order
--   of the remaining episodes in the list.
filterSeen :: [Episode] -- ^ List of already seen episodes.
           -> [Episode] -- ^ List of episodes to filter.
           -> [Episode]
filterSeen seen = flip (unorderedMinusBy orderEp) seen

-- | Like @\\@, but does not preserve the ordering of the resulting list.
--   Also removes every occurrences of [y <- ys] from xs, not just the first.
--   For instance, @unorderedMinusBy compare [x,x] [x] == []@.
unorderedMinusBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
unorderedMinusBy cmp as bs = go (sortBy cmp as) (sortBy cmp bs)
  where
    go xxs@(x:xs) yys@(y:ys) =
      case cmp x y of
        LT -> x : go xs yys
        GT -> go xxs ys
        EQ -> go xs yys
    go xs _ =
      xs

-- | Filter a list of episodes based on the episode filters in the given
--   config. The given list of episodes is assumed to only contain unseen
--   episodes, so no filtering based on the seen file is performed here.
filterEpisodes :: Config -> [Episode] -> [Episode]
filterEpisodes (Config {..}) allEps
    | cfgMatchLatest = take 1 $ sortBy compLatest eps
    | cfgAllowDupes  = eps
    | otherwise      = chooseEpisodes eps
  where
    compLatest = compareAll [ flip (compBy seasonNumber)
                            , flip (compBy episodeNumber)
                            , compBy resolution]
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
      | otherwise               = 1 `elem` cfgSeasons
    episodes (Episode {..})
      | Just e <- episodeNumber = e `elem` cfgEpisodes
      | otherwise               = False
    exts (Episode {..})
      | Just e <- fileExtension = e `elem` cfgExtensions
      | otherwise               = False
    groups (Episode {..})
      | Just g <- releaseGroup  = g `elem` cfgGroups
      | otherwise               = False
    resolutions (Episode {..})
      | Just r <- resolution    = r `elem` cfgResolutions
      | otherwise               = False

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
    criteria = [ flip (compBy resolution)
               , flip (compBy $ fmap (const "") . releaseGroup)
               , flip (compBy $ fmap (const "") . fileExtension)
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

orderEp :: Episode -> Episode -> Ordering
orderEp = compareAll [ \a b -> seriesName a `compare` seriesName b
                     , \a b -> maybe 1 id (seasonNumber a) `compare`
                               maybe 1 id (seasonNumber b)
                     , compBy episodeNumber
                     ]

sameEp :: Episode -> Episode -> Bool
sameEp a b = and [ seriesName a == seriesName b
                 , maybe 1 id (seasonNumber a) ==
                   maybe 1 id (seasonNumber b)
                 , eqBy episodeNumber a b
                 ]

-- | Compare two episodes on the given attribute. A missing attribute is
--   considered "less" than a present one.
compBy :: Ord a => (Episode -> Maybe a) -> Episode -> Episode -> Ordering
compBy f a b =
  case (f a, f b) of
    (Just x, Just y) -> x `compare` y
    (Just _, _)      -> GT
    (_, Just _)      -> LT
    _                -> EQ

-- | Compare two episodes for equality on the given attribute.
eqBy :: Eq a => (Episode -> Maybe a) -> Episode -> Episode -> Bool
eqBy f a b = maybe False id $ do
  x <- f a
  y <- f b
  pure (x == y)
