-- | RSS 2.0 feed source.
module Nyanbda.Sources.RSS (rssSource, rssHandler) where
import Control.Monad.IO.Class
import Control.Monad.Trans.Either (left)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Network.Curl.Download
import System.Console.GetOpt
import Text.Feed.Types
import Text.RSS.Syntax
import Nyanbda.Parser
import Nyanbda.Sources.Types
import Nyanbda.Types

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

-- | 
rssHandler :: ([String] -> [String]) -> SourceHandler
rssHandler mkURLs _ = do
    concat <$> mapM getFeedItems urls
  where
    getFeedItems url = do
      ef <- liftIO $ openAsFeed url
      case ef of
        Right (RSSFeed rss) -> mapM mkItem (rssItems $ rssChannel rss)
        Right _             -> left $ "feed `" ++ url ++ "' is not in " ++
                                      "RSS format"
        Left err            -> left $ "unable to get RSS feed `" ++ url ++
                                      "': " ++ err
    urls = mkURLs []

    mkItem item =
      case catMaybes [rssItemTitle item, rssItemDescription item] of
        [] -> left "RSS item has no title or description"
        xs -> pure $ (maximumBy completeness (map parseEpisode xs)) {
                  torrentLink = maybe "" id (rssItemLink item)
                }
