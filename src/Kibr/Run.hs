{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

-- | Support for configuration via dynamic recompilation using
-- "Config.Dyre".
--
-- For example, to configure @kibr@ to run the web application on port
-- 3000, write the following to the file @~\/.config\/kibr\/kibr.hs@:
--
-- >import Data.Configurable
-- >import Happstack.Server
-- >import Kibr.Run
-- >main = kibr conf {webServer = conf {port = 3000}}
--
-- If you delete your configuration, you may need to wipe @~\/.cache\/kibr@
-- as well.

module Kibr.Run
    ( -- * Main API for customization
      Config(..)
    , kibr
      -- * Command-line options
    , Options(..)
    , OutputMode(..)
    , TraceLevel
    , Command(..)
    , Service(..)
      -- ** Applicative option parsers
    , parseOptions
    , parseImport
    , parseCheckpoint
    , parseServe
    , parseLookup
      -- * Commands
    , Runtime(..)
    , ProgramT
    , Program
    , runProgramT
    , io
    , output
    , run
      -- * Compilation
    , CompilationError
    , main
    )
  where

import Prelude hiding ((.))
import Kibr.Data hiding (User)

import qualified Data.HashMap.Lazy as HashMap

import Config.Dyre                    (Params(..), wrapMain, defaultParams)
import Control.Category               ((.))
import Control.Exception              (bracket)
import Control.Monad                  (forM_, when)
import Control.Monad.Reader           (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Trans            (MonadIO, liftIO)
import Data.Acid                      (AcidState, openLocalStateFrom, closeAcidState, createCheckpoint)
import Data.Acid.Remote               (acidServer, openRemoteState)
import Data.Configurable              (Configurable(conf))
import Data.Default                   (def)
import Data.Maybe                     (fromMaybe)
import Data.Packable                  (fromList)
import Data.String                    (fromString)
import Happstack.Server               (Conf)
import Kibr.State
import Kibr.Text
import Kibr.XML                       (readDictionary)
import Network                        (HostName, PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..))
import Options.Applicative
import System.Environment             (getEnv)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit                    (exitFailure)
import System.FilePath                ((</>))
import System.IO                      (hPutStr, stdout, stderr)
import Text.InterpolatedString.Perl6  (qq)
import Text.PrettyPrint.ANSI.Leijen   (Doc, (<>), displayIO, renderPretty, plain, linebreak)
import Text.XML.HXT.Core              ((/>), runX, readDocument, withTrace)
import Text.XML.HXT.HTTP              (withHTTP)

#ifndef WINDOWS
import System.Console.Terminfo        (getCapability, termColumns, setupTermFromEnv)
import Text.XML.HXT.Expat             (withExpat)
#endif

-- | Application config.
data Config = Config
    { stateDirectory :: IO FilePath           -- ^ Path to /acid-state/ log directory.
    , stateServer    :: IO (HostName,PortID)  -- ^ Location of remote /acid-state/ server.
    , webServer      :: Conf                  -- ^ Configuration for the web server.
    , ircBots        :: [BotConf]             -- ^ IRC bots to run.
    }

instance Configurable Config where
    conf = Config
      { stateDirectory = getUserDataDir $ "kibr" </> "state"
      , stateServer    = do dir <- getEnv "XDG_RUNTIME_DIR"
                            return ("127.0.0.1",UnixSocket (dir </> "kibr-state.socket"))
      , webServer      = conf
      , ircBots        = [conf {nick = "kibr", host = "chat.freenode.net",
                                commandPrefix = "@", channels = fromList ["#sampla"],
                                user = conf {username = "kibr", realname = "Lojban IRC bot"}}]
      }

-- | The @kibr@ executable.
kibr :: Config -> IO ()
kibr cfg = run $ Right cfg
  where
    run = wrapMain $ defaultParams
        { projectName = "kibr"
        , showError   = const Left
        , realMain    = main
        }

-- | Command-line options.
data Options = Options
    { remote :: Bool
    , outputMode :: OutputMode
    , cmd :: Command
    }

-- | How to print output on the console.
data OutputMode = Colored | Plain | Quiet

-- | The level of debug output to print from the XML parser.
type TraceLevel = Int

-- | The sub-commands of the @kibr@ executable, and their individual
-- options.
data Command = Import TraceLevel FilePath
             | Checkpoint
             | Serve [Service]
             | Lookup Language [Word]

-- | Services that can be launched with @kibr serve@.
data Service = DICT | IRC | State | Web deriving (Eq, Bounded, Enum)

parseOptions :: Parser Options
parseOptions = Options
    <$> switch (long "remote" . help "Connect to remote state service")
    <*> nullOption
          ( reader (`lookup` outputModes)
          . long "output"
          . metavar "MODE"
          . value Colored
          . help "Control how output is printed (colored|plain|quiet)"
          )
    <*> subparser
          ( mkcmd "import"     parseImport     "Import words from an XML export"
          . mkcmd "checkpoint" parseCheckpoint "Create a state checkpoint"
          . mkcmd "serve"      parseServe      "Launch Internet services"
          . mkcmd "lookup"     parseLookup     "Look up words"
          )
  where
    mkcmd name parser desc = command name $ info (helper <*> parser) $ progDesc desc
    outputModes = [("colored",Colored), ("plain",Plain), ("quiet",Quiet)]

parseImport :: Parser Command
parseImport = Import
    <$> option
          ( long "trace-level"
          . metavar "0..4"
          . value 0
          . help "Set the tracing level for the XML parser"
          )
    <*> argument str (metavar "FILE")

parseCheckpoint :: Parser Command
parseCheckpoint = pure Checkpoint

parseServe :: Parser Command
parseServe = Serve
    <$> arguments (`lookup` services)
          ( metavar "dict|irc|state|web..."
          . value [minBound..]
          )
  where
    services = [("dict",DICT), ("irc",IRC), ("state",State), ("web",Web)]

parseLookup :: Parser Command
parseLookup = Lookup
    <$> nullOption
          ( reader ((`HashMap.lookup` languageTags) . fromString)
          . long "language"
          . metavar "TAG"
          . value (English UnitedStates)
          )
    <*> arguments (Just . fromString) (metavar "WORD...")

-- | An error message from GHC if /dyre/ fails to compile a user configuration.
type CompilationError = String

-- | The actual entry-point for the @kibr@ executable.
main :: Either CompilationError Config -> IO ()
main (Left msg) = hPutStr stderr msg >> exitFailure
main (Right config@Config{..}) = do
    options@Options{..} <- execParser parser
    bracket (openAcidState remote) closeAcidState $ \state ->
      runProgramT (run cmd) Runtime {config = config, options = options, state = state}
  where
    parser = info (helper <*> parseOptions) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def

-- | Runtime execution environment for @kibr@ commands.
data Runtime = Runtime
    { config :: Config
    , options :: Options
    , state  :: AcidState AppState
    }

-- | Monad transformer for monads with access to a 'Runtime' environment.
newtype ProgramT m a = Program (ReaderT Runtime m a)
                       deriving (Monad, MonadReader Runtime, MonadIO)

-- | Programs usually run in the 'IO' monad and produce no value.
type Program = ProgramT IO ()

-- | Run a 'ProgramT' with a 'Runtime' environment and return the inner
-- monad computation.
runProgramT :: Monad m => ProgramT m a -> Runtime -> m a
runProgramT (Program m) = runReaderT m

instance Monad m => HasAcidState (ProgramT m) AppState where
    getAcidState = asks state

-- | Convenient alias for 'liftIO'.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Document printer that is aware of the 'OutputMode' and the width of
-- the terminal if available.
output :: Doc -> Program
output doc = do
#if WINDOWS
    let width = Nothing
#else
    width <- io $ fmap (`getCapability` termColumns) setupTermFromEnv
#endif
    let prettyPrint = io . displayIO stdout . renderPretty 1.0 (fromMaybe 80 width)
    mode <- asks (outputMode . options)
    case mode of
      Colored -> prettyPrint (doc <> linebreak)
      Plain -> prettyPrint (plain doc <> linebreak)
      Quiet -> return ()

-- | Dispatches the commands.
run :: Command -> Program

run (Serve services) = do
    state <- asks state
    Config{..} <- asks config
    when (State `elem` services) $ io $
      do (_,port) <- stateServer
         acidServer state port

run (Import traceLevel doc) = do
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

run Checkpoint = io . createCheckpoint =<< asks state

run (Lookup language words) =
    forM_ words $ \word ->
      do Just typ <- query $ LookupWordType word
         Just def <- query $ LookupWordDefinition word language
         output $ linebreak <> ppWord word typ def
