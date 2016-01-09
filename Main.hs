import Control.Shell
import Control.Shell.Concurrent
import Control.Shell.Download
import System.Process (system)
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
-- TODO: populate "seen" cache using episodes stored on disk
main :: IO ()
main = shell_ $ do
  act <- parseConfig defaultConfig cmdline
  case act of
    SucceedWith s  -> echo s >> exit
    List   cfg str -> get True cfg str
    Get    cfg str -> get False cfg str

-- | Read the `seen' file, if any.
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
get :: Bool -> Config -> String -> Shell ()
get dryrun cfg str = do
    items <- search cfg str
    when (null items) $ fail "no matching items to download"

    if dryrun
      then echo "The following items would be downloaded:"
      else echo "The following items will be downloaded:"
    mapM_ (echo . ("  " ++) . episodeName cfg) items

    let cmds = case cfgExec cfg of
          Just cmd -> map (\ep -> cmd (mkFileName ep) ep) items
          _        -> []

    when (dryrun && not (null cmds)) $ do
      echo "Then the following commands would be executed:"
      mapM_ (echo . ("  " ++)) cmds

    unless dryrun $ do
      when (cfgInteractive cfg) $ do
        hPutStr stdout "Do you want to continue? [Y/n] " >> hFlush stdout
        unless ((`elem` ["y","Y",""]) <$> ask) exit

      -- Download torrents in batches of 13
      mapM_ parallel_ $ chunks 13 (map download items)

      -- Execute per torrent command, if applicable
      mapM_ (liftIO . system) cmds

      -- Save to seen file, if applicable
      case cfgSeenFile cfg of
        Just f -> liftIO $ appendFile f (unlines $ map (episodeName cfg) items)
        _      -> return ()
  where
    mkFileName ep = outdir </> episodeName cfg ep <.> "torrent"
    download ep = fetchFile (mkFileName ep) (torrentLink ep)
    outdir = maybe "." id (cfgOutdir cfg)
