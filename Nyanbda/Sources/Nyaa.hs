-- | NyaaTorrents torrent source.
module Nyanbda.Sources.Nyaa (nyaaSource) where
import System.Console.GetOpt
import Text.Parsec
import Text.Parsec.String
import Nyanbda.Parser
import Nyanbda.Sources.RSS
import Nyanbda.Sources.Types

-- | NyaaTorrents episode source. If no category is given, @anime-english@ is
--   assumed.
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
  | RawAnime
  | All
    deriving (Eq, Show)

-- | Turn a Nyaa category into a Nyaa search RSS URL.
urlFromCat :: NyaaCat -> String -> String
urlFromCat All          = ("http://www.nyaa.se/?page=rss&term=" ++)
urlFromCat AllAnime     = ("http://www.nyaa.se/?page=rss&cats=1_0&term=" ++)
urlFromCat EnglishAnime = ("http://www.nyaa.se/?page=rss&cats=1_37&term=" ++)
urlFromCat RawAnime     = ("http://www.nyaa.se/?page=rss&cats=1_11&term=" ++)

-- | Description for @--nyaa-cat@.
nyaaCatDesc :: String
nyaaCatDesc =
  "Use the given categories when searching NyaaTorrents. " ++
  "Valid categories: anime, anime-english, anime-raw, all. " ++
  "Multiple categories " ++
  "may be specified either as a comma-separated list, or by passing this " ++
  "option multiple times. If no category is specified, `anime-english' is " ++
  "assumed."

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
  , string "anime-raw"     *> pure RawAnime
  , string "all"           *> pure All
  ]

-- | Actual NyaaTorrents handler.
nyaaHandler :: ([NyaaCat] -> [NyaaCat]) -> SourceHandler
nyaaHandler mkCats search = rssHandler (const urls) search
  where
    cats = mkCats []
    urls
      | null cats = [urlFromCat EnglishAnime search]
      | otherwise = map (flip urlFromCat search) cats
