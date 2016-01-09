import Control.Shell
import Control.Shell.Concurrent
import Control.Shell.Download
import Nyanbda.Config
import Nyanbda.Filtering
import Nyanbda.Opts
import Nyanbda.Sources
import Nyanbda.Types
import Nyanbda.Parser (parseEpisode)

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

readSeenEpisodes :: Maybe FilePath -> Shell [Episode]
readSeenEpisodes (Just f) = do
  isf <- isFile f
  if isf
    then withFile f ReadMode $ \h -> do
      seen <- map parseEpisode . lines <$> hGetContents h
      length seen `seq` return seen
    else return []
readSeenEpisodes _ = do
  return []

-- | Perform an episode search using the given config and search term.
search :: Config -> String -> Shell [Episode]
search cfg str = do
    eps <- mapM (\h -> h str) handlers
    seen <- readSeenEpisodes (cfgSeenFile cfg)
    return . filterEpisodes cfg . filterSeen seen . concat $ eps
  where
    handlers = map srcHandler $ cfgSources cfg

-- | Print and download all episodes matching search and filters.
get :: Config -> String -> Shell ()
get cfg str = do
    items <- search cfg str
    when (null items) $ fail "no matching items to download"

    echo "The following items will be downloaded:"
    mapM_ (echo . ("  " ++) . episodeName cfg) items

    when (cfgInteractive cfg) $ do
      hPutStr stdout "Do you want to continue? [Y/n] " >> hFlush stdout
      unless ((`elem` ["y","Y",""]) <$> ask) exit
    inDirectory outdir $ mapM_ parallel_ $ chunks 13 (map download items)
    case cfgSeenFile cfg of
      Just f -> liftIO $ appendFile f (unlines $ map (episodeName cfg) items)
      _      -> return ()
  where
    download ep = fetchFile (episodeName cfg ep <.> "torrent") (torrentLink ep)
    outdir = maybe "." id (cfgOutdir cfg)
