module Nyanbda.Web.Config (getWebConfig, setWebConfig) where
import Data.IORef
import Nyanbda.Config
import System.IO.Unsafe

{-# NOINLINE activeWebConfig #-}
activeWebConfig :: IORef (FilePath, Config)
activeWebConfig = unsafePerformIO $ newIORef ("", defaultConfig)

setWebConfig :: FilePath -> Config -> IO ()
setWebConfig db cfg = writeIORef activeWebConfig (db, cfg)

getWebConfig :: IO (FilePath, Config)
getWebConfig = readIORef activeWebConfig
