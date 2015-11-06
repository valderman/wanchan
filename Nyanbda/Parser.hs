{-# LANGUAGE TupleSections, OverloadedStrings #-}
-- | Parse episode data from episode titles and file names.
module Nyanbda.Parser (
    -- * Parsing episode titles
    parseEpisode, parseEpisodeFrom,

    -- * Episode title formats
    pWesternFormat, pAnimeFormat
  ) where
import Control.Monad
import Data.Char
import Data.List
import Data.Text (pack)
import Network.Mime
import System.FilePath (takeExtension, dropExtension)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec
import Nyanbda.Types

-- | Parse an episode from either of the western or anime standard formats.
parseEpisode :: String -> Episode
parseEpisode s =
  case parseEpisodeFrom [pAnimeFormat, pWesternFormat] s of
    Just e -> e {fileExtension = takeKnownExtension s}
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

-- | Parse an episode title in the western style:
--   @Show name SxxEyy 720p-GROUP@
pWesternFormat :: Parser Episode
pWesternFormat = try $ do
  name <- pWesternName
  mse <- optionMaybe pWesternEpisode
  let season  = fst <$> mse
      episode = snd <$> mse
  spaces'
  res <- option Other pResolution
  skipMany $ noneOf "-"
  mgroup <- optionMaybe . try $ char '-' *> many1 (noneOf " [.-")
  pure $ nullEpisode {
      releaseGroup  = mgroup,
      resolution    = res,
      seriesName    = trim $ addSpaces $ fixExt season episode res name,
      seasonNumber  = season,
      episodeNumber = episode
    }

-- | Parse a western style series name: a sentence followed by a season/episode
--   number. Season + episode is not included, but only used as an end
--   marker.
pWesternName :: Parser String
pWesternName = manyTill' anyChar pWesternEpisode

-- | Like 'manyTill', but does not consume the terminator.
manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p end =
    (lookAhead (try end) *> pure []) <|> oneMore <|> eof *> pure []
  where
    oneMore = do
      x <- p
      xs <- manyTill' p end
      pure (x:xs)

-- | Parse a western format season/episode qualified: @SxxExx@.
pWesternEpisode :: Parser (Int, Int)
pWesternEpisode = try $ do
  void $ oneOf "sS"
  season <- integer
  void $ oneOf "eE"
  episode <- integer
  pure (season, episode)

-- | Parse an integer.
integer :: Parser Int
integer = try $ read <$> many1 digit

-- | Parses a show name in the standard anime release format:
--   @[Group] Show Name - 03 [resolution]@
--   Spaces may be replaced by underscores.
pAnimeFormat :: Parser Episode
pAnimeFormat = try $ do
    grp <- optionMaybe pTag
    spaces'
    (name, season, episode) <- pNameAndNumber
    skipMany $ noneOf "[("
    res <- firstRight Other . map (parse pResolution "") <$> many pTag
    pure $ nullEpisode {
        releaseGroup  = fmap trim grp,
        resolution    = res,
        seriesName    = trim $ addSpaces $ fixExt season episode res name,
        seasonNumber  = season,
        episodeNumber = episode
      }
  where
    firstRight _   (Right x : _) = x
    firstRight def (Left _ : xs) = firstRight def xs
    firstRight def []            = def

-- | Drop known file extension if season, episode and resolution are unknown.
fixExt :: Maybe Int -> Maybe Int -> Resolution -> String -> String
fixExt Nothing Nothing Other = dropKnownExtension
fixExt _ _ _                 = id


-- | Remove leading and trailing spaces.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Replace full stops and underscores by spaces.
addSpaces :: String -> String
addSpaces = map (\c -> if c == '.' || c == '_' then ' ' else c)

-- | Drop the file extension and trailing period from a file name for known
--   file extensions.
dropKnownExtension :: String -> String
dropKnownExtension s =
  case mimeByExt defaultMimeMap "" (pack s) of
    "" -> s
    _  -> dropExtension s

-- | Return the known file extension of a file, if any.
takeKnownExtension :: String -> Maybe String
takeKnownExtension s =
  case mimeByExt defaultMimeMap "" (pack s) of
    "" -> Nothing
    _  -> Just $ takeExtension s

-- | Zero or more spaces, underscores or full stops.
spaces' :: Parser ()
spaces' = void $ many space'

-- | A space character, an underscore, or a full stop; chars commonly used as
--   spaces in file names.
space' :: Parser Char
space' = space <|> oneOf "_."

-- | Read a series name and season + episode numbers.
pNameAndNumber :: Parser (String, Maybe Int, Maybe Int)
pNameAndNumber = try $ do
    name <- manyTill' anyChar (void pSeasonEpNumber <|> void pEpNumber)
    choice [ bothJust name <$> pSeasonEpNumber
           , (name, Nothing,) . Just <$> pEpNumber
           , pure (name, Nothing, Nothing) ]
  where
    bothJust name (x, y) = (name, Just x, Just y)

-- | Parse an episode number preceeded by a dash and at least one space.
pEpNumber :: Parser Int
pEpNumber = try $ many1 space' *> char '-' *> many1 space' *> integer

-- | Parse a season and an episode number separated by a dash and preceeded by
--   at least one space: @ Sx - yy@
pSeasonEpNumber :: Parser (Int, Int)
pSeasonEpNumber = try $ do
  void $ many1 space'
  season <- choice [ oneOf "sS" *> integer
                   , string "Season" *> spaces' *> integer
                   , string "season" *> spaces' *> integer ]
  void $ many1 space' *> char '-' *> many1 space'
  episode <- integer
  return (season, episode)

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
