import Control.Shell
import Control.Shell.Concurrent
import Control.Shell.Download
import Nyanbda.Config
import Nyanbda.Filtering
import Nyanbda.Opts
import Nyanbda.Sources
import Nyanbda.Types

-- TODO: interactive mode, allowing user to confirm/abort download
-- TODO: option to set sink for torrents
-- TODO: implement sink: external command
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
    List   cfg str -> void $ search cfg str >>= mapM_ (echo . episodeName cfg)
    Get    cfg str -> get cfg str

-- | Perform an episode search using the given config and search term.
search :: Config -> String -> Shell [Episode]
search cfg str = filterEpisodes cfg . concat <$> mapM (\h -> h str) handlers
  where
    handlers = map srcHandler $ cfgSources cfg

-- | Print and download all episodes matching search and filters.
get :: Config -> String -> Shell ()
get cfg str = do
    items <- search cfg str
    when (null items) $ fail "no matching items to download"

    echo "The following items will be downloaded:"
    mapM_ (echo . episodeName cfg) items
    inDirectory outdir $ parallel_ $ map download items
  where
    download ep = fetchFile (mkfn $ torrentLink ep) (torrentLink ep)
    outdir = maybe "." id (cfgOutdir cfg)
    mkfn u
      | '?' `elem` u = reverse (takeWhile (/= '=') $ reverse u) <.> "torrent"
      | otherwise    = takeFileName u
