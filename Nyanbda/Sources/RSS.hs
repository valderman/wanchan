-- | RSS 2.0 feed source.
module Nyanbda.Sources.RSS (rssSource, rssHandler) where
import Control.Shell
import Control.Shell.Download
import Control.Shell.Concurrent
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import Text.Feed.Types
import Text.RSS.Syntax
import Nyanbda.Parser
import Nyanbda.Sources.Types
import Nyanbda.Types
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (toString)

rssSource :: Source
rssSource = Source {
    srcName    = "rss",
    srcOpts    = [("url", rssUrlDesc, ReqArg setRSSUrl "URL")],
    srcHandler = rssHandler id
  }

-- | Description for @--rss-url@.
rssUrlDesc :: String
rssUrlDesc =
  "Fetch episode info from the given RSS feed URLs. Multiple feeds " ++
  "may be specified either as a comma-separated list, or by passing this " ++
  "option multiple times."

-- | Set the category of a NyaaTorrents source.
setRSSUrl :: String -> Source -> Either String Source
setRSSUrl urlstr src = do
  urls <- parseFlagVal pList "--rss-url" urlstr
  pure $ src {srcHandler = rssHandler (urls ++)}

-- | Fetch a list of items from an RSS feed.
rssHandler :: ([String] -> [String]) -> SourceHandler
rssHandler mkURLs _ = do
    concat . concat <$> mapM parallel (chunks 10 (map getFeedItems urls))
  where
    getFeedItems url = do
      ef <- try $ fetchFeed url
      case ef of
        Right (RSSFeed rss) -> mapM mkItem (rssItems $ rssChannel rss)
        Right _             -> fail $ "feed `" ++ url ++ "' is not in " ++
                                      "RSS format"
        Left err            -> fail $ "unable to get RSS feed `" ++ url ++
                                      "': " ++ err
    urls = mkURLs []

    mkItem item =
        case filter (not . null) candidates of
          []    -> fail "RSS item has no title or description"
          (x:_) -> pure ((parseEpisode $ utf8Fix x) {
                       torrentLink = maybe "" id (rssItemLink item)
                     })
      where
        candidates = catMaybes [rssItemTitle item, rssItemDescription item]
        -- feed library messes up UTF8, we need to fix it before parsing
        utf8Fix = toString . pack
