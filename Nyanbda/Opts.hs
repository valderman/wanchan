-- | ADT describing the command line options to 
module Nyanbda.Opts (Action (..), parseConfig) where
import Control.Monad
import Control.Monad.Trans.Either
import Data.List (sortBy, intercalate)
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import Text.Parsec
import Text.Parsec.String
import Nyanbda.Config
import Nyanbda.Parser
import Nyanbda.Sources

-- | The action to be taken by the main program.
data Action
  = SucceedWith String
  | FailWith    String
  | Search      Config String

-- | An option set by a config file or on the command line.
data Option
  = ReadConfig FilePath
  | SetFlag (Config -> Either String Config)
  | SetSourceFlag (Source -> Either String Source)
  | SetAction (Config -> String -> Action)

opts :: [OptDescr Option]
opts =
  [ Option "s" ["season"]      (ReqArg addSeasons "SEASON") $
    "Only match episodes from the given SEASON. SEASON may be either an " ++
    "integer or a range of integers given as 'a..b'. This option may be " ++
    "given several times to specify multiple seasons."
  , Option "e" ["episode"]     (ReqArg addEpisodes "EPISODE") $
    "Only match the given EPISODEs. EPISODE may be either an " ++
    "integer or a range of integers given as 'a..b'. This option may be " ++
    "given several times to specify multiple episodes."
  , Option "c" ["config"]      (ReqArg ReadConfig "FILE") $
    "Read the given configuration file before applying command line " ++
    "options. If this option is given multiple times, the configuration " ++
    "files will be read in order from left to right. " ++
    "By default, only ~/.config/nyanbda/nyan.conf will be read, if it exists."
  , Option "l" ["latest"]      (OptArg getLatest "yes/no") $
    "Match only the latest episode of the series. If one or more " ++
    "seasons are given, the latest episode of each season will be " ++
    "matched. Use --latest=no to disable this criterion; the latest " ++
    "episode(s) may still be matched by other criteria."
  , Option "d" ["allow-duplicates"] (OptArg allowDupes "yes/no") $
    "Allow several copies of the same episode, but with different " ++
    "resolution, release group, etc. By default, only one of each episode " ++
    "is allowed."
  , Option "a" ["all"]         (NoArg clearAllMatches) $
    "Match any episode. Useful to override more specific matches set in " ++
    "configuration files or previously on the command line."
  , Option "g" ["group"]       (ReqArg addGroup "GROUP") $
    "Match only episodes from the given GROUP. If this option is given " ++
    "multiple times, all indicated groups are considered acceptable."
  , Option "r" ["resolution"]  (ReqArg addRes "RES") $
    "Match only episodes with the given resolution. Valid values are " ++
    "1080p, 720p and 480p. Use this option multiple times to indicate that" ++
    "multiple resolutions are acceptable."
  , Option "t" ["type"]        (ReqArg addType "EXT") $
    "Match only episodes with the given file extension. Use this option " ++
    "multiple times to indicate that multiple file types are acceptable. " ++
    "If EXT has the special value 'any', any previously set file types " ++
    "criteria are cleared."
  , Option "f" ["from"]        (ReqArg addSources "SOURCE") $
    "Search only the given SOURCE. This option may be given several times " ++
    "to search multiple sources. Valid sources are " ++
    intercalate ", " supportedSourceNames ++ ". " ++
    "If no source is specified, all supported sources are searched."
  , Option "o" ["outdir"]      (ReqArg setOutdir "DIR") $
    "Download the corresponding torrent file for each matched episode to " ++
    "the given DIRectory. If no DIR is given, the current working " ++
    "directory is used."
  , Option "h?" ["help"]       (NoArg printHelp) "Display this message."
  ] ++ supportedSourceOpts

-- | Print the help message, then exit.
printHelp :: Option
printHelp = SetAction $ \_ _ -> SucceedWith helpMessage
  where
    helpMessage = unlines $
      [ "Usage: nyan [OPTIONS] SEARCH STRING"
      , ""
      , "Effortlessly manage downloads of TV series and other media."
      , ""
      ] ++ map helpString opts

-- | Generate a help message, padded to 80 characters.
helpString :: OptDescr a -> String
helpString (Option short long opt help) =
    shorts ++ longs ++ "\n" ++ formatHelpMessage 80 help
  where
    (longarg, shortarg) =
      case opt of
        NoArg _    -> ("", "")
        ReqArg _ a -> ('=':a, ' ':a)
        OptArg _ a -> ("[=" ++ a ++ "]", " [" ++ a ++ "]")
    shorts =
      case intercalate ", " (map (\c -> ['-',c]) short) of
        s | null s    -> ""
          | otherwise -> s ++ shortarg ++ ", "
    longs =
      case intercalate ", " (map (\s -> "--" ++ s) long) of
        l | null l    -> ""
          | otherwise -> l ++ longarg

-- | Break lines at n chars, add two spaces before each.
formatHelpMessage :: Int -> String -> String
formatHelpMessage chars help =
    unlines . map ("    " ++) . breakLines 0 [] $ words help
  where
    breakLines len ln (w:ws)
      | length w >= chars-4     = w:unwords (reverse ln):breakLines 0 [] ws
      | len+length w >= chars-4 = unwords (reverse ln):breakLines 0 [] (w:ws)
      | otherwise               = breakLines (len+1+length w) (w:ln) ws
    breakLines _ ln _ =
      [unwords $ reverse ln]

-- | Create a configuration from a list of command line arguments and a default
--   config.
parseConfig :: Config -> [String] -> IO Action
parseConfig c args = do
    ecfg <- runEitherT $ do
      unless (null errs) $ left (concat errs)
      cfg <- mkConfig c os
      return (applyDefaults cfg, unwords nonopts)
    case ecfg of
      Left err        -> pure $ FailWith err
      Right (cfg, search)
        | null search -> pure $ findAction noSearchStr (reverse os) cfg search
        | otherwise   -> pure $ findAction Search (reverse os) cfg search
  where
    applyDefaults cfg
      | null (cfgSources cfg) =
        applyDefaults (cfg {cfgSources = supportedSources})
      | otherwise =
        cfg

    (os, nonopts, errs) = getOpt RequireOrder opts args
    noSearchStr _ _ = FailWith "no search string given\n"

    findAction _   (SetAction act : _) = act
    findAction def (_:xs)              = findAction def xs
    findAction def _                   = def

-- | Create a configuration from a list of parsed options and a default config.
mkConfig :: Config -> [Option] -> EitherT String IO Config
mkConfig c os = do
    foldM modifyConfig c $ sortBy (\a b -> optIx a `compare` optIx b) os
  where
    optIx (ReadConfig {})    = 0 :: Int
    optIx (SetFlag {})       = 1
    optIx (SetSourceFlag {}) = 2
    optIx (SetAction {})     = 3

    modifyConfig _cfg (ReadConfig _f)  = left "TODO: reading config files"
    modifyConfig cfg (SetFlag f)       = hoistEither $ f cfg
    modifyConfig cfg (SetSourceFlag f) = do
      let srcs = case cfgSources cfg of
                   [] -> supportedSources
                   ss -> ss
      srcs' <- hoistEither $ mapM f srcs
      pure $ cfg {cfgSources = srcs'}
    modifyConfig cfg (SetAction {})    = pure cfg

-- | Parse an integer range. A single integer qualifies as a singleton range.
pIntRange :: Parser [Int]
pIntRange = do
  m <- integer
  n <- try (spaces *> string ".." *> spaces *> integer) <|> pure m
  return [m..n]

-- | Parse a supported source name.
pSourceName :: Parser Source
pSourceName = choice $ map parsify supportedSources
  where
    parsify :: Source -> Parser Source
    parsify source = string (srcName source) *> pure source

-- | All supported source-specific options.
supportedSourceOpts :: [OptDescr Option]
supportedSourceOpts =
    concat $ map (\s -> map (mkOpt (srcName s)) (srcOpts s)) supportedSources
  where
    toSrcFlag (NoArg f)    = NoArg (SetSourceFlag f)
    toSrcFlag (ReqArg f s) = ReqArg (\x -> SetSourceFlag (f x)) s
    toSrcFlag (OptArg f s) = OptArg (\x -> SetSourceFlag (f x)) s
    mkOpt n (opt, desc, argspec) =
      Option "" [n ++ "-" ++ opt] (toSrcFlag argspec) desc

-- | Names of all supported sources.
supportedSourceNames :: [String]
supportedSourceNames = map srcName supportedSources

-- | Add a range of seasons to match.
addSeasons :: String -> Option
addSeasons s = SetFlag $ \c -> do
  ss <- parseFlagVal pIntRange "--season" s
  pure $ c {cfgSeasons = snub $ ss ++ cfgSeasons c}

-- | Add a range of episodes to match.
addEpisodes :: String -> Option
addEpisodes e = SetFlag $ \c -> do
  es <- parseFlagVal pIntRange "--episode" e
  pure $ c {cfgEpisodes = snub $ es ++ cfgEpisodes c}

-- | Add an acceptable release group.
addGroup :: String -> Option
addGroup g = SetFlag $ \c -> do
  gs <- parseFlagVal pList "--group" g
  pure $ c {cfgGroups = snub $ gs ++ cfgGroups c}

-- | Add an acceptable resolution.
addRes :: String -> Option
addRes r = SetFlag $ \c -> do
    rs <- parseFlagVal resList "--resolution" r
    if Nothing `elem` rs
      then pure $ c {cfgResolutions = []}
      else pure $ c {cfgResolutions = snub $ catMaybes rs++cfgResolutions c}
  where
    resList =
      (optionMaybe pResolution <|> (string "any" *> pure Nothing))
        `sepBy1` char ','

-- | Add an acceptable file format.
addType :: String -> Option
addType t = SetFlag $ \c -> do
  ts <- parseFlagVal pList "--type" t
  if "any" `elem` ts
    then pure $ c {cfgExtensions = []}
    else pure $ c {cfgExtensions = snub $ ts ++ cfgExtensions c}

-- | Clear all previous constraints.
clearAllMatches :: Option
clearAllMatches = SetFlag $ \c -> pure $ c {
    cfgExtensions = [],
    cfgResolutions = [],
    cfgGroups = [],
    cfgSeasons = [],
    cfgEpisodes = []
  }

-- | Allow duplicate episodes?
allowDupes :: Maybe String -> Option
allowDupes (Just "no") = SetFlag $ \c -> pure c {cfgAllowDupes = False}
allowDupes _           = SetFlag $ \c -> pure c {cfgAllowDupes = True}

-- | Always match the latest episode?
getLatest :: Maybe String -> Option
getLatest (Just "no") = SetFlag $ \c -> pure c {cfgMatchLatest = False}
getLatest _           = SetFlag $ \c -> pure c {cfgMatchLatest = True}

-- | Add one or more torrent sources.
addSources :: String -> Option
addSources src = SetFlag $ \c -> do
    srcs <- parseFlagVal pSources "--source" src
    if null srcs
      then pure $ c {cfgSources = []}
      else pure $ c {cfgSources = snub $ srcs ++ cfgSources c}
  where
    pSources = choice
      [ pSourceName `sepBy1` char ','
      , string "all" *> pure []
      ]

-- | Set the output directory.
setOutdir :: FilePath -> Option
setOutdir dir = SetFlag $ \c -> pure c {cfgOutdir = Just dir}
