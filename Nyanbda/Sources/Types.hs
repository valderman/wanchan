-- | Basic torrent source types.
module Nyanbda.Sources.Types (Source (..), SourceOpt, SourceHandler) where
import Control.Monad.Trans.Either
import System.Console.GetOpt
import Nyanbda.Types

-- | Describes a torrent source.
data Source = Source {
    srcName    :: String,
    srcOpts    :: [SourceOpt],
    srcHandler :: SourceHandler
  }

instance Show Source where
  show = srcName

-- | Function handling a given torrent source.
--   A source handler accepts a search string as an argument, and returns the
--   list of episodes returned for the given search.
type SourceHandler = String -> EitherT String IO [Episode]

-- | A source specific option.
type SourceOpt = (String, String, ArgDescr (Source -> Either String Source))

instance Eq Source where
  a == b = srcName a == srcName b

instance Ord Source where
  a `compare` b = srcName a `compare` srcName b
