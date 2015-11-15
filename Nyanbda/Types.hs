{-# LANGUAGE RecordWildCards #-}
-- | Basic types representing episodes.
module Nyanbda.Types where

type URL = String

-- | Resolution of a video file.
data Resolution = HD1080 | HD720 | SD480
  deriving (Show, Eq, Ord)

data Episode = Episode {
    -- | The release group behind this episode.
    releaseGroup  :: Maybe String,

    -- | Resolution of this video file.
    resolution    :: Maybe Resolution,

    -- | Name of the series the episode belongs to.
    seriesName    :: String,

    -- | The season number of this episode.
    seasonNumber  :: Maybe Int,

    -- | If this the first, second, etc. episode of its season?
    episodeNumber :: Maybe Int,

    -- | URL of a .torrent file for this episode.
    torrentLink   :: URL,

    -- | Extension of the file, if known.
    fileExtension :: Maybe String
  } deriving Show

-- | An episode with no information whatsoever.
nullEpisode :: Episode
nullEpisode = Episode {
    releaseGroup  = Nothing,
    resolution    = Nothing,
    seriesName    = "",
    seasonNumber  = Nothing,
    episodeNumber = Nothing,
    torrentLink   = "",
    fileExtension = Nothing
  }

-- | Count the number of complete fields in an 'Episode'.
countComplete :: Episode -> Int
countComplete e =
    sum [ oneIf releaseGroup
        , oneIf (mstr . seriesName)
        , oneIf (mstr . torrentLink)
        , oneIf resolution
        , oneIf seasonNumber
        , oneIf episodeNumber]
  where
    mstr "" = Nothing
    mstr n  = Just n
    oneIf field = maybe 0 (const 1) (field e)

-- | Compare two episodes for completeness of information.
completeness :: Episode -> Episode -> Ordering
completeness a b = countComplete a `compare` countComplete b

-- | Serialise an episode name in standard anime format.
episodeNameAnime :: Episode -> String
episodeNameAnime Episode {..} =
    concat [group, seriesName, season, episode, res]
  where
    group   = maybe "" (\g -> "[" ++ g ++ "] ") releaseGroup
    season  = maybe "" ((" S" ++) . show) seasonNumber
    episode = maybe "" ((" - " ++) . pad 2) episodeNumber
    res     = maybe "" (\r -> " [" ++ r ++ "]") (showResolution <$> resolution)

-- | Serialise an episode name in standard anime format.
episodeNameWestern :: Episode -> String
episodeNameWestern Episode {..} =
    concat [map toDot seriesName, episode, res, group]
  where
    toDot ' ' = '.'
    toDot c   = c
    group = maybe "" ('-':) releaseGroup
    episode =
      case (seasonNumber, episodeNumber) of
        (Just s, Just e) -> concat [".S", pad 2 s, "E", pad 2 e]
        (Just s, _)      -> concat [".Season.", show s]
        (_, Just e)      -> concat [".Episode.", show e]
        _                -> ""
    res = maybe "" ('.':) (fmap showResolution resolution)

-- | Show an integer padded with at most @minlen@ leading zeroes.
pad :: Int -> Int -> String
pad minlen n = replicate (minlen-length s) '0' ++ s
  where s = show n

-- | Human readable resolution.
showResolution :: Resolution -> String
showResolution HD1080 = "1080p"
showResolution HD720  = "720p"
showResolution SD480  = "480p"
