import System.Environment
import System.Exit
import Nyanbda.Config
import Nyanbda.Opts

main :: IO ()
main = do
  args <- getArgs
  ecfg <- parseConfig defaultConfig args
  case ecfg of
    Left err            -> putStr err >> exitFailure
    Right (cfg, search) -> do
      putStrLn $ "Search string: " ++ search
      putStrLn $ "Config: " ++ show cfg
