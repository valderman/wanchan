module Nyanbda.Parser where
import Control.Monad
import Data.List
import Data.Char
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec
import Nyanbda.Types

-- | Parse an episode from either of the western or anime standard formats.
parseEpisode :: String -> Episode
parseEpisode s =
  case parseEpisodeFrom [pAnimeFormat, pWesternFormat] s of
    Just e -> e
    _      -> error $ "Impossibly failed to parse episode: " ++ s

-- | Parse an episode from one of the given formats. If more than one parser
--   matches, the parser which manages to fill in the most fields takes
--   precedence. In the case of a tie, the parser specified first in the list
--   of formats is choosen.
parseEpisodeFrom :: [Parser Episode] -> String -> Maybe Episode
parseEpisodeFrom formats s =
  case [ep | fmt <- formats, Right ep <- [parse fmt "" s]] of
    [] -> Nothing
    xs -> Just $ maximumBy completeness (reverse xs)

-- | Count the number of complete fields in an 'Episode'.
countComplete :: Episode -> Int
countComplete e =
    sum [ oneIf releaseGroup
        , oneIf (mstr . seriesName)
        , oneIf (mstr . torrentLink)
        , oneIf (mres . resolution)
        , oneIf seasonNumber
        , oneIf episodeNumber]
  where
    mres Other = Nothing
    mres r     = Just r
    mstr "" = Nothing
    mstr n  = Just n
    oneIf field = maybe 0 (const 1) (field e)

-- | Compare two episodes for completeness of information.
completeness :: Episode -> Episode -> Ordering
completeness a b = countComplete a `compare` countComplete b

-- | Parse an episode title in the western style:
--   @Show name SxxEyy 720p-GROUP@
pWesternFormat :: Parser Episode
pWesternFormat = try $ do
  name <- pWesternName
  mse <- optionMaybe pWesternEpisode
  spaces'
  mres <- optionMaybe pResolution
  skipMany $ noneOf "-"
  mgroup <- optionMaybe . try $ char '-' *> many1 (noneOf " [.-")
  pure $ Episode {
      releaseGroup  = mgroup,
      resolution    = maybe Other id mres,
      seriesName    = trim name,
      seasonNumber  = fst <$> mse,
      episodeNumber = snd <$> mse,
      torrentLink   = ""
    }

-- | Parse a western style series name: a sentence followed by a season/episode
--   number. Season + episode is not included, but only used as an end
--   marker.
pWesternName :: Parser String
pWesternName = trim . map fixString <$> manyTill' anyChar pWesternEpisode
  where
    fixString '.' = ' '
    fixString c   = c

-- | Like 'manyTill', but does not consume the terminator.
manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p end =
    (lookAhead (try end) *> pure []) <|> oneMore
  where
    oneMore = do
      x <- p
      xs <- manyTill' p end
      pure (x:xs)

-- | Parse a western format season/episode qualified: @SxxExx@.
pWesternEpisode :: Parser (Int, Int)
pWesternEpisode = try $ do
  void $ oneOf "sS"
  season <- many1 digit
  void $ oneOf "eE"
  episode <- many1 digit
  pure (read season, read episode)

-- | Parses a show name in the standard anime release format:
--   @[Group] Show Name - 03 [resolution]@
--   Spaces may be replaced by underscores.
pAnimeFormat :: Parser Episode
pAnimeFormat = try $ do
    grp <- optionMaybe pTag
    spaces'
    (name, number) <- pNameAndNumber []
    skipMany $ noneOf "[("
    res <- firstRight Other . map (parse pResolution "") <$> many pTag
    pure $ Episode {
        releaseGroup  = fmap trim grp,
        resolution    = res,
        seriesName    = trim name,
        seasonNumber  = Nothing,
        episodeNumber = number,
        torrentLink   = ""
      }
  where
    firstRight _   (Right x : _) = x
    firstRight def (Left _ : xs) = firstRight def xs
    firstRight def []            = def

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

spaces' :: Parser ()
spaces' = void $ many space'

space' :: Parser Char
space' = choice [space, char '_', char '.']

-- | Read a series name and an episode number.
--   Attempts to read the longest episode name possible while still being able
--   to figure out an episode number.
pNameAndNumber :: [String] -> Parser (String, Maybe Int)
pNameAndNumber pre = try $ do
    name <- many (noneOf "-")
    mr <- optionMaybe . try . lookAhead $ do
      res <- char '-' *> pNameAndNumber (name:pre)
      pos <- getPosition
      pure (pos, res)
    case mr of
      Just (pos, r@(_, Just _)) -> do
        -- We found a name and an episode number
        setPosition pos
        pure r
      Just (pos, (name', _)) -> do
        -- No episode number, but we did find a name - try to parse a number
        -- from what's left of the input
        setPosition pos
        choice [ try $ char '-' *> spaces' *> nameAndNum name (many1 digit)
               , pure (name', Nothing) ]
      _ ->
        -- Found nothing, so stop here
        pure (intercalate "-" (reverse (name : pre)), Nothing)
  where
    nameAndNum name pNum = do
      num <- pNum
      pure (intercalate "-" (reverse (name : pre)), Just $ read num)

-- | Parse a resolution string.
--   If no resolution is recognized, the parser will fail.
--   Thus, 'Other' will never be returned.
pResolution :: Parser Resolution
pResolution =
    choice [hd1080, hd720, sd480]
  where
    res :: String -> String -> Resolution -> Parser Resolution
    res w h r = do
      void $ choice
        [ try (string h <* oneOf "pP")
        , try (string w <* spaces' <* char 'x'<* spaces' <* string h) ]
      pure r
    hd1080 = res "1920" "1080" HD1080
    hd720  = res "1280" "720"  HD720
    sd480  = res "848"  "480"  SD480

-- | Parse a bracketed tag. Commonly used for translation/release group names.
pTag :: Parser String
pTag = choice [ try $ char '[' *> many1 (noneOf "]") <* char ']'
              , try $ char '(' *> many1 (noneOf ")") <* char ')' ]
