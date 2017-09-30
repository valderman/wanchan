module Wanchan.Web.Config (getWebConfig, setWebConfig) where
import Data.IORef
import Wanchan.Config
import System.IO.Unsafe
import Database.Selda.Backend

{-# NOINLINE activeWebConfig #-}
activeWebConfig :: IORef (SeldaConnection, Config)
activeWebConfig = unsafePerformIO $ newIORef (undefined, defaultConfig)

setWebConfig :: SeldaConnection -> Config -> IO ()
setWebConfig db cfg = writeIORef activeWebConfig (db, cfg)

getWebConfig :: IO (SeldaConnection, Config)
getWebConfig = readIORef activeWebConfig
