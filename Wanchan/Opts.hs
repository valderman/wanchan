-- | ADT describing the command line options to 
module Wanchan.Opts (Action (..), parseConfig) where
import Control.Shell
import Data.List (sortBy, intercalate)
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import Text.Parsec as P
import Text.Parsec.String
import Wanchan.Config
import Wanchan.Parser
import Wanchan.Sources
import Wanchan.Types

-- | The action to be taken by the main program.
data Action
  = SucceedWith String
  | List        Config String
  | Get         Config String
  | Batch       Config String
  | Daemon      Int Config String
  | WebDaemon   Int Config String

-- | An option set by a config file or on the command line.
data Option
  = ReadConfig FilePath
  | SetFlag (Config -> Either String Config)
  | SetSourceFlag (Source -> Either String Source)
  | SetAction (Config -> String -> Action)
  | Group [Option]

opts :: [Either String (OptDescr Option)]
opts =
  [ Left "Actions"
  , Right $ Option "A" ["daemon"]      (ReqArg setDaemon "MINS") $
    "Daemon mode: this mode is equivalent to batch mode, re-run " ++
    "every MINS minutes. Implies `--force'."
  , Right $ Option "B" ["batch"]       (NoArg (setAction Batch)) $
    "Run in batch mode: all command line arguments are interpreted as " ++
    "files containing one `search specifications' per line." ++
    "A search specification consists of all command line filtering options " ++
    "and non-option arguments. The following search specification would " ++
    "match all HorribleSubs releases of the third season of YuruYuri:" ++
    "\n\n    yuruyuri -s3 -gHorribleSubs\n\n" ++
    "Any filtering options passed on the command line are used as default " ++
    "overridden by batch files."
  , Right $ Option "D" ["dry-run"]     (NoArg (setAction List)) $
    "List all actions that would be performed by --get. " ++
    "This is the default action."
  , Right $ Option "G" ["get"]         (NoArg (setAction Get)) $
    "Download all torrents matching the given search string and filters, " ++
    "passing it to all specified output handlers."
  , Right $ Option "L" ["list"]        (NoArg listWithDupes) $
    "List all torrents matching the search string and filters. " ++
    "This is the default action. Implies --allow-duplicates."
  , Right $ Option "W" ["web-daemon"]  (ReqArg setWebDaemon "MINS") $
    "Check for updates to watched series every MINS minutes. " ++
    "Unlike daemon mode, watched series are not read from batch files, " ++
    "but added using a web interface."

  , Left "Filtering options"
  , Right $ Option "s" ["season"]      (ReqArg addSeasons "SEASON") $
    "Only match episodes from the given SEASON. SEASON may be either an " ++
    "integer or a range of integers given as 'a..b'. This option may be " ++
    "given several times to specify multiple seasons."
  , Right $ Option "e" ["episode"]     (ReqArg setEpisodes "EPISODE") $
    "Only match the given EPISODEs. EPISODE may be either an " ++
    "integer or a range of integers given as 'a..b'."
  , Right $ Option "l" ["latest"]      (OptArg getLatest "yes/no") $
    "Match only the latest episode of the series. If there is more than " ++
    "one season of the matching series, the latest episode of the latest " ++
    "season will be matched. " ++
    "Use --latest=no to disable this criterion; the latest " ++
    "episode may still be matched by other criteria."
  , Right $ Option "d" ["allow-duplicates"] (OptArg allowDupes "yes/no") $
    "Allow several copies of the same episode, but with different " ++
    "resolution, release group, etc. By default, only one of each episode " ++
    "is allowed."
  , Right $ Option "a" ["all"]         (NoArg clearAllMatches) $
    "Match any episode. Useful to override more specific matches set in " ++
    "configuration files or previously on the command line."
  , Right $ Option "g" ["group"]       (ReqArg addGroup "GROUP") $
    "Match only episodes from the given GROUP. If this option is given " ++
    "multiple times, all indicated groups are considered acceptable."
  , Right $ Option "r" ["resolution"]  (ReqArg addRes "RES") $
    "Match only episodes with the given resolution. Valid values are " ++
    "1080p, 720p and 480p. Use this option multiple times to indicate that" ++
    "multiple resolutions are acceptable."
  , Right $ Option "t" ["type"]        (ReqArg addType "EXT") $
    "Match only episodes with the given file extension. Use this option " ++
    "multiple times to indicate that multiple file types are acceptable. " ++
    "If EXT has the special value 'any', any previously set file types " ++
    "criteria are cleared."

  , Left "Output options"
  , Right $ Option "o" ["outdir"]      (ReqArg setOutdir "DIR") $
    "Download the corresponding torrent file for each matched episode to " ++
    "the given DIRectory. If no DIR is given, the current working " ++
    "directory is used."
  , Right $ Option "x" ["exec"]        (ReqArg setExec "CMD") $
    "Execute CMD for each matching episode, where CMD is a shell " ++
    "command. Subsitution is performed on the following format strings in " ++
    "CMD:\n\n" ++ unlines
      [ "    %f  Local path to torrent file; only valid with `--get'."
      , "    %u  Torrent URL"
      , "    %n  Series name"
      , "    %e  Episode number"
      , "    %s  Season number"
      , "    %g  Release group"
      , "    %r  Resolution"
      , "    %t  File type"
      , "    %a  Anime style episode name"
      , "    %w  Western style episode name"
      , "    %%  A literal `%' character"
      ]
  , Right $ Option "n" ["anime-style"]     (NoArg animeStyle) $
    "Print episode names in anime style: [Group] Title Sx - yy [resolution]."++
    " This is the default."
  , Right $ Option "w" ["western-style"]   (NoArg westernStyle) $
    "Print episode names in western style: Title.SxxEyy.resolution-group."

  , Left "Torrent source options"
  , Right $ Option "" ["from"]        (ReqArg addSources "SOURCE") $
    "Search only the given SOURCE. This option may be given several times " ++
    "to search multiple sources. Valid sources are " ++
    intercalate ", " supportedSourceNames ++ ". " ++
    "If no source is specified, all supported sources are searched."
  ]
  ++ supportedSourceOpts ++
  [ Left "Web daemon options"
  , Right $ Option "p" ["port"]        (ReqArg setPort "PORT") $
    "Listen for incoming HTTP connections on PORT." ++
    "Only applies to web daemon mode."
  , Right $ Option "U" ["user"]        (ReqArg setUser "USER") $
    "Require USER as the username to access the web interface." ++
    "Only applies to web daemon mode."
  , Right $ Option "P" ["password"]    (ReqArg setPass "PASS") $
    "Require PASS as the password to access the web interface." ++
    "Only applies to web daemon mode."
  , Left "Misc. options"
  , Right $ Option "b"  ["database"]   (ReqArg setDatabase "FILE") $
    "Use FILE as this sessions database file. Episodes present in the " ++
    "database's seen episodes list will be considered already seen, " ++
    "and thus not included in search results. " ++
    "Any episodes downloaded as a result of a invocation " ++
    "using this option will be appended to the seen list as well." ++
    "Additionally, in web daemon mode, the list of series to watch for" ++
    "is also stored in this database."
  , Right $ Option "i" ["interactive"] (NoArg (setInteractive True)) $
    "Prompt the user before downloading files. " ++
    "This is the default behavior."
  , Right $ Option "f" ["force"]       (NoArg (setInteractive False)) $
    "Don't prompt the user before downloading files."
  , Right $ Option "h?" ["help"]       (NoArg printHelp) "Display this message."
  ]


-- | Print the help message, then exit.
printHelp :: Option
printHelp = setAction $ \_ _ -> SucceedWith helpMessage
  where
    helpMessage = unlines $
      [ "Usage: nyan [OPTIONS] SEARCH STRING"
      , ""
      , "Effortlessly manage downloads of TV series and other media."
      , ""
      ] ++ map helpString opts

-- | Generate a help message, padded to 80 characters.
helpString :: Either String (OptDescr a) -> String
helpString (Left subheading) =
    subheading ++ "\n"
helpString (Right (Option short long opt help)) =
    concat
      [ shorts
      , longs
      , "\n"
      , concat (map (formatHelpMessage 80) (lines help))
      ]
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
    unlines . map ("    " ++) . breakLines 0 [] $ words' help
  where
    breakLines len ln (w:ws)
      | length w >= chars-4     = w:unwords (reverse ln):breakLines 0 [] ws
      | len+length w >= chars-4 = unwords (reverse ln):breakLines 0 [] (w:ws)
      | otherwise               = breakLines (len+1+length w) (w:ln) ws
    breakLines _ ln _ =
      [unwords $ reverse ln]

words' :: String -> [String]
words' "" = []
words' s
  | (xs, ' ':ys) <- break (== ' ') s = xs : words' ys
  | otherwise                        = [s]

-- | Create a configuration from a list of command line arguments and a default
--   config.
parseConfig :: Config -> [String] -> Shell Action
parseConfig cfg args = do
    unless (null errs) $ fail (concat errs)
    cfg' <- applyDefaults <$> mkConfig cfg os
    if null search
      then findAction noSearchStr (reverse os) cfg' search
      else findAction (\c s -> pure $ List c s) (reverse os) cfg' search
  where
    applyDefaults c
      | null (cfgSources c) =
        applyDefaults (c {cfgSources = supportedSources})
      | otherwise =
        c

    (os, nonopts, errs) = getOpt Permute [o | Right o <- opts] args
    search = unwords nonopts
    noSearchStr _ _ = fail "no search string/batch files given\n"

    findAction _   (SetAction act : _) c s = pure (act c s)
    findAction def (_:xs) c s              = findAction def xs c s
    findAction def _ c s                   = def c s

-- | Create a configuration from a list of parsed options and a default config.
mkConfig :: Config -> [Option] -> Shell Config
mkConfig c os = do
    foldM modifyConfig c $ sortBy (\a b -> optIx a `compare` optIx b) os'
  where
    os' = concatMap flatten os
    flatten (Group xs) = xs
    flatten x          = [x]

    optIx (ReadConfig {})    = 0 :: Int
    optIx (SetFlag {})       = 1
    optIx (SetSourceFlag {}) = 2
    optIx (SetAction {})     = 3
    optIx (Group {})         = error "Eliminate Group first!"

    modifyConfig _   (ReadConfig _f)   = fail "TODO: config files"
    modifyConfig cfg (SetFlag f)       = hoistEither $ f cfg
    modifyConfig cfg (SetSourceFlag f) = do
      let srcs = case cfgSources cfg of
                   [] -> supportedSources
                   ss -> ss
      srcs' <- hoistEither $ mapM f srcs
      pure $ cfg {cfgSources = srcs'}
    modifyConfig cfg (SetAction {})    = pure cfg
    modifyConfig _   (Group {})        = error "Eliminate Group first!"

    hoistEither (Left e)  = fail e
    hoistEither (Right r) = pure r

-- | Parse an integer range. A single integer qualifies as a singleton range.
pIntRange :: Parser (Int, Int)
pIntRange = do
  m <- integer
  n <- P.try (spaces *> string ".." *> spaces *> integer) <|> pure m
  return (m, n)

-- | Parse an integer range as a list. A single integer qualifies as a singleton range.
pIntList :: Parser [Int]
pIntList = do
  (m, n) <- pIntRange
  return [m..n]

-- | Parse a supported source name.
pSourceName :: Parser Source
pSourceName = choice $ map parsify supportedSources
  where
    parsify :: Source -> Parser Source
    parsify source = string (srcName source) *> pure source

-- | All supported source-specific options.
supportedSourceOpts :: [Either String (OptDescr Option)]
supportedSourceOpts =
    concat $ map mkSrcOpts supportedSources
  where
    mkSrcOpts s =
        Left ("Options for source `" ++ srcName s ++ "'") : sopts
      where
        sopts = map (mkOpt (srcName s)) (srcOpts s)
    toSrcFlag (NoArg f)    = NoArg (SetSourceFlag f)
    toSrcFlag (ReqArg f s) = ReqArg (\x -> SetSourceFlag (f x)) s
    toSrcFlag (OptArg f s) = OptArg (\x -> SetSourceFlag (f x)) s
    mkOpt n (opt, desc, argspec) =
      Right $ Option "" [n ++ "-" ++ opt] (toSrcFlag argspec) desc

-- | Names of all supported sources.
supportedSourceNames :: [String]
supportedSourceNames = map srcName supportedSources

-- | Set the action to be performed by this invocation.
setAction :: (Config -> String -> Action) -> Option
setAction = SetAction

-- | Perform a search, allowing dupes.
listWithDupes :: Option
listWithDupes = Group [setAction List, allowDupes (Just "yes")]

-- | Add a range of seasons to match.
addSeasons :: String -> Option
addSeasons s = SetFlag $ \c -> do
  ss <- parseFlagVal pIntList "--season" s
  pure $ c {cfgSeasons = snub $ ss ++ cfgSeasons c}

-- | Set the range of episodes to match.
setEpisodes :: String -> Option
setEpisodes e = SetFlag $ \c -> do
  es <- parseFlagVal pIntRange "--episode" e
  pure $ c {cfgEpisodes = es}

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
    cfgEpisodes = (0, 1000000000)
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

-- | Run in daemon mode.
setDaemon :: String -> Option
setDaemon t = setAction (Daemon (read t))

-- | Run in web daemon mode.
setWebDaemon :: String -> Option
setWebDaemon t = setAction (WebDaemon (read t))

-- | Set port for web daemon mode.
setPort :: String -> Option
setPort p = SetFlag $ \c -> pure c {cfgHttpPort = read p}

-- | Set user name for web daemon mode.
setUser :: String -> Option
setUser u = SetFlag $ \c -> pure c {cfgWebUser = u}

-- | Set password for web daemon mode.
setPass :: String -> Option
setPass p = SetFlag $ \c -> pure c {cfgWebPassword = p}

-- | Set command to execute for each episode.
setExec :: String -> Option
setExec cmd = SetFlag $ \c -> pure c {cfgExec = Just $ parseExec cmd}

split :: Char -> String -> [String]
split c = go ""
  where
    go acc (x:xs)
      | x == c    = reverse acc : go "" xs
      | otherwise = go (x:acc) xs
    go [] _       = []
    go acc _      = [reverse acc]

parseExec :: String -> FilePath -> Episode -> String
parseExec cmd p ep =
    concat $ first : subst parts
  where
    (first : parts) = split '%' cmd
    subst ("":xs)      = "%" : subst xs
    subst (('f':s):xs) = "\"" : p : "\"" : s : subst xs
    subst (('u':s):xs) = torrentLink ep : s : subst xs
    subst (('n':s):xs) = "\"" : seriesName ep : "\"" : s : subst xs
    subst (('e':s):xs) = show (episodeNumber ep) : s : subst xs
    subst (('s':s):xs) = show (seasonNumber ep) : s : subst xs
    subst (('g':s):xs) = "\"" : maybe "" id (releaseGroup ep) : "\"":s:subst xs
    subst (('r':s):xs) = show (resolution ep) : s : subst xs
    subst (('t':s):xs) = maybe "" id (fileExtension ep) : s : subst xs
    subst (('a':s):xs) = "\"" : episodeNameAnime ep : "\"" : s : subst xs
    subst (('w':s):xs) = "\"" : episodeNameWestern ep : "\"" : s : subst xs
    subst (x:xs)       = "%" : x : subst xs
    subst _            = []

-- | Set the session's database file.
setDatabase :: FilePath -> Option
setDatabase file = SetFlag $ \c -> pure c {cfgDatabase = Just file}

-- | Set anime style name display.
animeStyle :: Option
animeStyle = SetFlag $ \c -> pure c {cfgNameStyle = episodeNameAnime}

-- | Set western style name display.
westernStyle :: Option
westernStyle = SetFlag $ \c -> pure c {cfgNameStyle = episodeNameWestern}

-- | Set the interactive flag on/off.
setInteractive :: Bool -> Option
setInteractive i = SetFlag $ \c -> pure c {cfgInteractive = i}
