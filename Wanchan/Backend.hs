{-# LANGUAGE TemplateHaskell #-}
module Wanchan.Backend (runMain, batch, search, get) where
import qualified Control.Concurrent as CC (threadDelay, forkIO)
import Control.Shell
import Control.Shell.Concurrent
import Control.Shell.Download
import Data.List (intercalate)
import System.Process (system)
import Wanchan.Config
import Wanchan.Filtering
import Wanchan.Opts
import Wanchan.Sources
import Wanchan.Types
import Wanchan.Database
import Database.Selda.SQLite
import Database.Selda.Backend (runSeldaT, SeldaConnection)

-- TODO: make this conditional
import Wanchan.Web
import Wanchan.Web.Config
import Wanchan.Web.HttpServer
import Data.FileEmbed
import qualified Data.ByteString as BS

-- | Main entry point of application.
runMain :: Config -> [String] -> Shell ()
runMain defconfig cmd = do
  act <- parseConfig defconfig cmd
  case act of
    SucceedWith s  -> echo s >> exit
    List   cfg str -> initDB cfg >> get Nothing True cfg str
    Get    cfg str -> initDB cfg >> get Nothing False cfg str
    Batch  cfg str -> initDB cfg >> batch cfg (words str)
    Daemon t cfg str -> initDB cfg >> daemon t cfg (words str)
    WebDaemon t cfg _ -> webDaemon t cfg
  where
    initDB = liftIO . maybe (pure ()) (flip withSQLite initialize) . cfgDatabase

-- | Run a list of searches in batch mode.
batch :: Config -> [FilePath] -> Shell ()
batch cfg files = do
    when (cfgInteractive cfg) $ do
      go True
      echo_ "Do you want to continue? [Y/n] "
      getStdOut >>= hFlush
      unless ((`elem` ["y","Y",""]) <$> ask) exit
    go False
  where
    mkOpts True line  = words line ++ ["--dry-run"]
    mkOpts False line = words line ++ ["--get", "--force"]
    go dryrun = do
      forM_ files $ \file -> do
        lns <- lines <$> input file
        parallel_ $ flip map lns $ \line -> do
          void . try $ runMain cfg (mkOpts dryrun line)

-- | Perform an episode search using the given config and search term.
search :: Maybe SeldaConnection -> Config -> String -> Shell [Episode]
search mconn cfg str = do
    eps <- filterEpisodes cfg . concat <$> mapM (\h -> h str) handlers
    case (mconn, cfgDatabase cfg) of
      (Just db, _) -> unsafeLiftIO $ runSeldaT (filterSeen eps) db
      (_, Just db) -> unsafeLiftIO $ withSQLite db (filterSeen eps)
      _            -> pure eps
  where
    handlers = map srcHandler $ cfgSources cfg

-- | Print and download all episodes matching search and filters.
get :: Maybe SeldaConnection -> Bool -> Config -> String -> Shell ()
get mconn dryrun cfg str = do
    items <- search mconn cfg str
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
        echo_ "Do you want to continue? [Y/n] "
        getStdOut >>= hFlush
        unless ((`elem` ["y","Y",""]) <$> ask) exit

      -- Download torrents in batches of 13
      mapM_ parallel_ $ chunks 13 (map download items)

      -- Execute per torrent command, if applicable
      mapM_ (liftIO . system) cmds

      -- Save to seen file, if applicable
      case (mconn, cfgDatabase cfg) of
        (Just db, _) -> unsafeLiftIO $ runSeldaT (addSeen items) db
        (_, Just db) -> unsafeLiftIO $ withSQLite db (addSeen items)
        _            -> return ()
  where
    mkFileName ep = outdir </> episodeName cfg ep <.> "torrent"
    download ep = fetchFile (mkFileName ep) (torrentLink ep)
    outdir = maybe "." id (cfgOutdir cfg)

-- | Run in web daemon mode: like daemon mode, but get watch list from database
--   provided by web interface.
webDaemon :: Int -> Config -> Shell ()
webDaemon minutes cfg = do
    db <- unsafeLiftIO $ case cfgDatabase cfg of
      Just f -> putStrLn ("using database `" ++ f ++ "'") >> sqliteOpen f
      _      -> putStrLn "using temporary in-memory database" >> sqliteOpen "wanchan.sqlite"
    unsafeLiftIO $ setWebConfig db cfg
    echo $ "Scheduling updates every " ++ show minutes ++ " minutes."
    unsafeLiftIO $ runSeldaT initialize db
    unsafeLiftIO $ do
      CC.forkIO $ serve (fromIntegral $ cfgHttpPort cfg) assets
      CC.threadDelay 1000
    forkIO $ update db
    unsafeLiftIO $ webMain
  where
    update db = do
      series <- unsafeLiftIO $ runSeldaT allWatched db
      mapM_ (check db) series
      echo $ "Done! Next run in " ++ show minutes ++ " minutes."
      unsafeLiftIO $ wait minutes
      update db

    check db series = do
      let cfg' = cfg
            { cfgResolutions = [seriesResolution series]
            , cfgSeasons = [seriesSeason series]
            , cfgEpisodes = (0, 1000000000)
            , cfgGroups = [seriesGroup series]
            , cfgInteractive = False
            }
          name = Wanchan.Database.seriesName series
      echo $ "Checking for new episodes of " ++ name ++ "..."
      void . try $ get (Just db) False cfg' name

    assets req = do
      case uriPath $ rqURI req of
        "/"            -> respond $(embedFile "assets/index.html")
        "/index.html"  -> respond $(embedFile "assets/index.html")
        "/WebMain.js"  -> respond $(embedFile "assets/WebMain.js")
        "/wanchan.css" -> respond $(embedFile "assets/wanchan.css")
        _              -> notFound BS.empty

-- | Run in daemon mode: like batch mode, but re-run every n minutes.
daemon :: Int -> Config -> [FilePath] -> Shell ()
daemon minutes cfg files = do
    echo $ "Scheduling batch runs every " ++ show minutes ++ " minutes."
    echo $ "Batch files used: " ++ intercalate ", " files
    case cfgDatabase cfg of
      Just f -> echo $ "Database used: " ++ f
      _      -> return ()
    go
  where
    go = do
      batch (cfg {cfgInteractive = False}) files
      unsafeLiftIO $ wait minutes
      go

wait :: Int -> IO ()
wait mins
  | mins <= 10 = CC.threadDelay (mins*60*1000000)
  | otherwise  = CC.threadDelay (10*60*1000000) >> wait (mins-10)
