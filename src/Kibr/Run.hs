{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards, QuasiQuotes #-}

-- | Support for configuration via dynamic recompilation using
-- "Config.Dyre".
--
-- For example, to configure @kibr@ to run the web application on port
-- 3000, write the following to the file @~\/.config\/kibr\/kibr.hs@:
--
-- >import Happstack.Server
-- >import Kibr.CLI
-- >import Kibr.Run
-- >main = kibr baseConfig{webServer = nullConf{port = 3000}}
--
-- If you delete your configuration, you may need to wipe @~\/.cache\/kibr@
-- as well.

module Kibr.Run
    ( -- * Dynamic recompilation
      kibr
    , CompilationError
    , main
      -- * Configuration
    , baseConfig
    , botConfig
      -- * Applicative option parsers
    , enumValues
    , enumNames
    , enumVars
    , enumOption
    , enumArguments
    , valueAll
    , parseOptions
    , parseImport
    , parseCheckpoint
    , parseServe
    , parseLookup
      -- * Commands
    , program
    , runServe
    , runImport
    , runCheckpoint
    , runLookup
    , runSearch
    )
  where

import Prelude   hiding (catch)
import Kibr.Data hiding (User)

import qualified Data.Acid         as Acid
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set          as Set
import qualified Data.Text         as Text

import Config.Dyre                    (Params(..), wrapMain, defaultParams)
import Control.Arrow                  ((&&&))
import Control.Concurrent             (forkIO, killThread)
import Control.Exception              (bracket, catch, throw)
import Control.Monad                  (forM_, when, unless)
import Control.Monad.Reader           (asks)
import Data.Acid                      (AcidState, openLocalStateFrom, createCheckpoint)
import Data.Acid.Remote               (acidServer, openRemoteState)
import Data.Char                      (toLower)
import Data.Default                   (def)
import Data.List                      (intercalate)
import Data.String                    (fromString)
import Data.Text                      (Text)
import Happstack.Server               (nullConf)
import Happstack.Server.SimpleHTTP    (waitForTermination)
import Kibr.CLI
import Kibr.State
import Kibr.Text                      (ppWord, ppWords)
import Kibr.XML                       (readDictionary)
import Network                        (PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..), nullBotConf, nullUser)
import Options.Applicative
import Options.Applicative.Types      (Completer(..))
import System.Directory               (createDirectoryIfMissing, removeFile)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit                    (exitFailure)
import System.FilePath                ((</>))
import System.IO                      (hPutStr, stderr)
import System.IO.Error                (isDoesNotExistError)
import Text.InterpolatedString.Perl6  (qq)
import Text.PrettyPrint.ANSI.Leijen   ((<>), linebreak)
import Text.XML.HXT.Core              ((/>), runX, readDocument, withTrace)
import Text.XML.HXT.HTTP              (withHTTP)

#ifndef WINDOWS
import Text.XML.HXT.Expat             (withExpat)
#endif


-- * Dynamic recompilation
-- ***************************************************************************

-- | The @kibr@ executable.
kibr :: Config -> IO ()
kibr cfg = run $ Right cfg
  where
    run = wrapMain defaultParams
        { projectName = "kibr"
        , showError   = const Left
        , realMain    = main
        }

-- | An error message from GHC if /dyre/ fails to compile a user configuration.
type CompilationError = String

-- | The actual entry-point for the @kibr@ executable.
main :: Either CompilationError Config -> IO ()
main (Left msg) = hPutStr stderr msg >> exitFailure
main (Right config@Config{..}) = do
    options <- execParser parser
    state   <- openAcidState config
    runProgramT program Runtime{..}
  where
    parser = info (helper <*> parseOptions config) fullDesc

openAcidState :: Config -> IO (AcidState AppState)
openAcidState Config{..} =
    openRemote `catch` tryLocal
  where
    openRemote = do (host,port) <- stateServer
                    openRemoteState host port
    openLocal  = do dir <- stateDirectory
                    openLocalStateFrom dir def
    tryLocal e = do unless (isDoesNotExistError e) $ throw e
                    openLocal


-- * Configuration
-- ***************************************************************************

-- | Default configuration.
baseConfig :: Config
baseConfig = Config
    { stateDirectory = getUserDataDir $ "kibr" </> "state"
    , stateServer    = state
    , webServer      = nullConf
    , ircBots        = [botConfig]
    }
  where
    state = do dir <- getRuntimeDir "kibr"
               createDirectoryIfMissing True dir
               return ("127.0.0.1",UnixSocket (dir </> "state"))

-- | Default bot.
botConfig :: BotConf
botConfig = nullBotConf
    { nick = "kibr"
    , host = "chat.freenode.net"
    , commandPrefix = "@"
    , channels = Set.fromList ["#sampla"]
    , user = nullUser{username = "kibr", realname = "Lojban IRC bot"}
    }


-- * Applicative option parsers
-- ***************************************************************************

enumValues :: (Bounded a, Enum a, Show a) => [(String,a)]
enumValues = map (map toLower . show &&& id) [minBound..]

enumNames :: (Enum a, Show a) => a -> [String]
enumNames a = map (map toLower . show) [a..]

enumVars :: (Enum a, Show a) => a -> String
enumVars a = intercalate "|" $ enumNames a

enumOption :: (Bounded a, Enum a, Show a) => Mod OptionFields a -> Parser a
enumOption mod = nullOption (reader (`lookup` enumValues) & mod)

enumArguments :: (Bounded a, Enum a, Show a) => Mod ArgumentFields [a] -> Parser [a]
enumArguments = arguments (`lookup` enumValues)

valueAll :: (Bounded a, Enum a) => Mod f [a]
valueAll = value [minBound..]

parseOptions :: Config -> Parser Options
parseOptions config = Options
    <$> nullOption
          ( reader ((`HashMap.lookup` languageTags) . fromString)
          & long "language"
          & metavar "TAG"
          & value (English UnitedStates)
          & completeWith (map (Text.unpack . unLanguageTag) $ HashMap.keys languageTags)
          & help "Select language of dictionary"
          )
    <*> enumOption
          ( long "output"
          & metavar "MODE"
          & value TTY
          & completeWith (enumNames TTY)
          & help [qq|Control how output is printed ({enumVars TTY})|]
          )
    <*> subparser
          ( mkcmd "import"     parseImport          "Import words from an XML export"
          & mkcmd "checkpoint" parseCheckpoint      "Create a state checkpoint"
          & mkcmd "serve"      parseServe           "Launch Internet services"
          & mkcmd "lookup"     (parseLookup config) "Look up words"
          & mkcmd "search"     parseSearch          "Search words in definitions"
          )
  where
    mkcmd name parser desc = command name $ info (helper <*> parser) $ progDesc desc

parseImport :: Parser Command
parseImport = Import
    <$> option
          ( long "trace-level"
          & metavar "0..4"
          & value 0
          & help "Set the tracing level for the XML parser"
          )
    <*> argument str (metavar "FILE" & action "file")

parseCheckpoint :: Parser Command
parseCheckpoint = pure Checkpoint

parseServe :: Parser Command
parseServe = Serve
    <$> enumArguments
          ( valueAll
          & metavar [qq|{enumVars DICT}...|]
          & completeWith (enumNames DICT)
          )

parseLookup :: Config -> Parser Command
parseLookup config = Lookup
    <$> arguments (Just . Word . Text.replace "h" "'" . fromString)
          ( metavar "WORD..."
          & completer (Completer complete)
          )
  where
    complete str = do state <- openAcidState config
                      words <- Acid.query state $ CompleteWords (Text.pack str)
                      return (map (Text.unpack . unWord) $ Set.toList words)

parseSearch :: Parser Command
parseSearch = Search <$> arguments (Just . fromString) (metavar "KEYWORD...")


-- * Commands
-- ***************************************************************************

-- | Dispatches the commands.
program :: Program
program = do
    cmd <- asks (cmd . options)
    case cmd of
      Serve services        -> runServe services
      Import traceLevel doc -> runImport traceLevel doc
      Checkpoint            -> runCheckpoint
      Lookup words          -> runLookup words
      Search keywords       -> runSearch keywords

runServe :: [Service] -> Program
runServe services = do
    state <- asks state
    Config{..} <- asks config
    when (State `elem` services) $ io $
      do (_,port) <- stateServer
         bracket (forkIO $ acidServer state port)
                 (\tid ->
                   do killThread tid
                      case port of
                        UnixSocket path -> tryRemoveFile path
                        _ -> return ())
                 (const waitForTermination)
  where
    tryRemoveFile fp  = removeFile fp `catch` ignoreNotExists
    ignoreNotExists e = unless (isDoesNotExistError e) $ throw e

runImport :: TraceLevel -> FilePath -> Program
runImport traceLevel doc = do
    dict <- io $ runX $ readDocument sys doc /> readDictionary
    output "Importing..."
    update_ $ ImportWords dict
    let total = sum $ map (length . snd) dict
    output [qq|Finished importing $total words.|]
  where
    sys = [withHTTP [], withTrace traceLevel]
#ifndef WINDOWS
            ++ [withExpat True]
#endif

runCheckpoint :: Program
runCheckpoint = io . createCheckpoint =<< asks state

runLookup :: [Word] -> Program
runLookup words = do
    language <- asks (language . options)
    forM_ words $ \word ->
      do Just typ <- query $ LookupWordType word
         Just def <- query $ LookupWordDefinition word language
         output $ linebreak <> ppWord word typ def

runSearch :: [Text] -> Program
runSearch keywords = do
    language <- asks (language . options)
    words <- query $ SearchKeyWords (map (KeyWord language . DefinitionWord) keywords)
    output $ ppWords words
