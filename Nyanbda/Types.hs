-- | Basic types representing episodes.
module Nyanbda.Types where

type URL = String

-- | Resolution of a video file.
data Resolution = HD1080 | HD720 | SD480 | Other
  deriving Show

data Episode = Episode {
    -- | The release group behind this episode.
    releaseGroup  :: Maybe String,

    -- | Resolution of this video file.
    resolution    :: Resolution,

    -- | Name of the series the episode belongs to.
    seriesName    :: String,

    -- | The season number of this episode.
    seasonNumber  :: Maybe Int,

    -- | If this the first, second, etc. episode of its season?
    episodeNumber :: Maybe Int,

    -- | URL of a .torrent file for this episode.
    torrentLink   :: URL
  } deriving Show
