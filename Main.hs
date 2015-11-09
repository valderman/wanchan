import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import System.Environment
import System.Exit
import Nyanbda.Config
import Nyanbda.Opts
import Nyanbda.Sources
import Nyanbda.Types

main :: IO ()
main = do
  args <- getArgs
  act <- parseConfig defaultConfig args
  case act of
    SucceedWith s  -> putStr s >> exitSuccess
    FailWith s     -> putStr s >> exitFailure
    Search cfg str -> search cfg str

-- | Perform an episode search using the given config and search term.
search :: Config -> String -> IO ()
search cfg str = do
    res <- runEitherT $ do
      items <- concat <$> mapM (\h -> h str) handlers
      mapM_ (liftIO . putStrLn . episodeNameAnime) items
    case res of
      Left err -> putStr err >> exitFailure
      _        -> return ()
  where
    handlers = map srcHandler $ cfgSources cfg
