import System.Environment
import System.Exit
import Nyanbda.Config
import Nyanbda.Opts

main :: IO ()
main = do
  args <- getArgs
  act <- parseConfig defaultConfig args
  case act of
    SucceedWith s  -> putStr s >> exitSuccess
    FailWith s     -> putStr s >> exitFailure
    Search cfg str -> putStrLn "TODO: implement search"
