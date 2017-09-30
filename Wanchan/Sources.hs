-- | Wanchan torrent sources.
module Wanchan.Sources (Source (..), SourceOpt, supportedSources) where
import Wanchan.Sources.Types
import Wanchan.Sources.Nyaa
import Wanchan.Sources.RSS

-- | A list of all supported sources.
supportedSources :: [Source]
supportedSources = [nyaaSource, rssSource]
