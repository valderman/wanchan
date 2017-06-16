-- | NyaaTorrents torrent source.
module Nyanbda.Sources.Nyaa (nyaaSource) where
import Data.Bits
import Data.Char
import Data.Word (Word8)
import System.Console.GetOpt
import Text.Parsec
import Text.Parsec.String
import Nyanbda.Parser
import Nyanbda.Sources.RSS
import Nyanbda.Sources.Types
import Nyanbda.Types (Episode (..))

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

-- | Borrowed from @HTTP@ to avoid having to pull in the entire package.
urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t) 
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (encodeChar ch)
  | otherwise = escape (fromIntegral (fromEnum ch)) (urlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)

     showH :: Word8 -> String -> String
     showH x xs
       | x <= 9    = to (o_0 + x) : xs
       | otherwise = to (o_A + (x-10)) : xs
      where
       to  = toEnum  .  fromIntegral
       fro = fromIntegral . fromEnum

       o_0 = fro '0'
       o_A = fro 'A'

encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- | Turn a Nyaa category into a Nyaa search RSS URL.
urlFromCat :: NyaaCat -> String -> String
urlFromCat All          = ("https://www.nyaa.si/?page=rss&q=" ++)
urlFromCat AllAnime     = ("https://www.nyaa.si/?page=rss&c=1_0&q=" ++)
urlFromCat EnglishAnime = ("https://www.nyaa.si/?page=rss&c=1_2&q=" ++)
urlFromCat RawAnime     = ("https://www.nyaa.si/?page=rss&c=1_4&q=" ++)

-- | Turn a Nyaa category and an offset into a Nyaa search RSS URL.
rssUrl :: NyaaCat -> Int -> String -> String
rssUrl cat n search
  | n > 1     = base ++ "&f=" ++ show n
  | otherwise = base
  where
    base = urlFromCat cat (urlEncode search)

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
    pages = 5
    cats = mkCats []
    urls
      | null cats = [rssUrl EnglishAnime pg search | pg <- [1..pages]]
      | otherwise = [rssUrl cat pg search | cat <- cats, pg <- [1..pages]]
