import Control.Shell
import Nyanbda.Config
import Nyanbda.Filtering
import Nyanbda.Opts
import Nyanbda.Sources
import Nyanbda.Types

-- TODO: implement download action (separate from searching)
-- TODO: dry run for download action
-- TODO: interactive mode, allowing user to confirm/abort download
-- TODO: option to set sink for torrents
-- TODO: implement sinks: download and external command
-- TODO: implement per series filtering; we may not want the same group for
--       each series, etc.
-- TODO: implement scheduler/watcher daemon
-- TODO: implement "seen" functionality, to avoid downloading previously seen
--       episodes
-- TODO: config flag to force download of seen episodes
-- TODO: populate "seen" cache using episodes stored on disk
-- TODO: config flag to clean "seen" cache
main :: IO ()
main = shell_ $ do
  act <- parseConfig defaultConfig cmdline
  case act of
    SucceedWith s  -> echo s >> exit
    List   cfg str -> void $ search cfg str


-- | Perform an episode search using the given config and search term.
search :: Config -> String -> Shell [Episode]
search cfg str = do
    items <- filterEpisodes cfg . concat <$> mapM (\h -> h str) handlers
    mapM_ (liftIO . putStrLn . episodeNameAnime) items
    return items
  where
    handlers = map srcHandler $ cfgSources cfg
