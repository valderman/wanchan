-- | NyaaTorrents torrent source.
module Nyanbda.Sources.Nyaa (nyaaSource) where
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import System.Console.GetOpt
import Text.Parsec
import Text.Parsec.String
import Nyanbda.Parser
import Nyanbda.Sources.Types
import Nyanbda.Types

nyaaSource :: Source
nyaaSource = Source {
    srcName    = "nyaa",
    srcOpts    = [("cat", nyaaCatDesc, ReqArg setNyaaCat "CAT")],
    srcHandler = nyaaHandler id
  }

-- | Supported NyaaTorrents categories.
data NyaaCat
  = AllAnime
  | EnglishAnime
  | All
    deriving (Eq, Show)

-- | Description for @--nyaa-cat@.
nyaaCatDesc :: String
nyaaCatDesc =
  "Use the given categories when searching NyaaTorrents. " ++
  "Valid categories: anime, anime-english, all. Multiple categories " ++
  "may be specified either as a comma separated list, or by passing this " ++
  "option multiple times."

-- | Set the category of a NyaaTorrents source.
setNyaaCat :: String -> Source -> Either String Source
setNyaaCat c src = do
  cs <- parseFlagVal (pNyaaCat `sepBy1` char ',') "--nyaa-cat" c
  pure $ src {srcHandler = nyaaHandler (cs ++)}

-- | Parse a NyaaTorrents category.
pNyaaCat :: Parser NyaaCat
pNyaaCat = choice
  [ string "anime"         *> pure AllAnime
  , string "anime-english" *> pure EnglishAnime
  , string "all"           *> pure All
  ]

-- | Actual NyaaTorrents handler.
nyaaHandler :: ([NyaaCat] -> [NyaaCat]) -> String -> EitherT String IO [Episode]
nyaaHandler mkCats search = do
    liftIO (mapM_ print cats)
    liftIO $ putStrLn search
    return []
  where
    cats = mkCats []
