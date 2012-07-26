{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings #-}

-- | Command-line interface support code.
module Kibr.CLI
    ( -- * Configuration
      Config(..)
      -- * Command-line options
    , Options(..)
    , Command(..)
    , TraceLevel
    , Service(..)
      -- ** Applicative option parsers
    , parseOptions
    , parseImport
    , parseCheckpoint
    , parseServe
    , parseLookup
      -- * Printing output
    , OutputMode(..)
    , output
      -- * Programs
    , Runtime(..)
    , ProgramT
    , Program
    , runProgramT
      -- * Utilities
    , io
    )
  where

import Kibr.Data hiding (User)

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set          as Set
import qualified Happstack.Server  as Happstack

import Control.Monad.Reader           (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Trans            (MonadIO, liftIO)
import Data.Acid                      (AcidState)
import Data.Conf                      (Conf(conf))
import Data.Has                       (Has(fetch))
import Data.Maybe                     (fromMaybe)
import Data.String                    (fromString)
import Data.Text                      (Text)
import Kibr.State
import Network                        (HostName, PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..))
import Options.Applicative
import System.Environment             (getEnvironment)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserCacheDir)
import System.FilePath                ((</>))
import System.IO                      (stdout, hIsTerminalDevice)
import Text.PrettyPrint.ANSI.Leijen   (Doc, (<>), displayIO, renderPretty, plain, linebreak)

#ifndef WINDOWS
import System.Console.Terminfo        (getCapability, termColumns, setupTermFromEnv)
#endif


-- * Configuration
-- ***************************************************************************

-- | Application config.
data Config = Config
    { stateDirectory :: IO FilePath           -- ^ Path to /acid-state/ log directory.
    , stateServer    :: IO (HostName,PortID)  -- ^ Location of remote /acid-state/ server.
    , webServer      :: Happstack.Conf        -- ^ Configuration for the web server.
    , ircBots        :: [BotConf]             -- ^ IRC bots to run.
    }

instance Conf Config where
    conf = Config
      { stateDirectory = getUserDataDir $ "kibr" </> "state"
      , stateServer    = do xdg <- lookup "XDG_RUNTIME_DIR" <$> getEnvironment
                            dir <- maybe (getUserCacheDir "kibr") return xdg
                            return ("127.0.0.1",UnixSocket (dir </> "kibr-state.socket"))
      , webServer      = conf
      , ircBots        = [conf{nick = "kibr", host = "chat.freenode.net",
                               commandPrefix = "@", channels = Set.fromList ["#sampla"],
                               user = conf{username = "kibr", realname = "Lojban IRC bot"}}]
      }


-- * Command-line options
-- ***************************************************************************

-- | Command-line options.
data Options = Options
    { remote     :: Bool
    , outputMode :: OutputMode
    , cmd        :: Command
    }

-- | The sub-commands of the @kibr@ executable, and their individual
-- options.
data Command = Import TraceLevel FilePath
             | Checkpoint
             | Serve [Service]
             | Lookup Language [Word]
             | Search Language [Text]

-- | The level of debug output to print from the XML parser.
type TraceLevel = Int

-- | Services that can be launched with @kibr serve@.
data Service = DICT | IRC | State | Web deriving (Eq, Bounded, Enum)


-- * Applicative option parsers
-- ***************************************************************************

parseOptions :: Parser Options
parseOptions = Options
    <$> switch (long "remote" & help "Connect to remote state service")
    <*> nullOption
          ( reader (`lookup` outputModes)
          & long "output"
          & metavar "MODE"
          & value TTY
          & help "Control how output is printed (tty|colored|plain|quiet)"
          )
    <*> subparser
          ( mkcmd "import"     parseImport     "Import words from an XML export"
          & mkcmd "checkpoint" parseCheckpoint "Create a state checkpoint"
          & mkcmd "serve"      parseServe      "Launch Internet services"
          & mkcmd "lookup"     parseLookup     "Look up words"
          & mkcmd "search"     parseSearch     "Search words in definitions"
          )
  where
    mkcmd name parser desc = command name $ info (helper <*> parser) $ progDesc desc
    outputModes = [("tty",TTY), ("colored",Colored), ("plain",Plain), ("quiet",Quiet)]

parseImport :: Parser Command
parseImport = Import
    <$> option
          ( long "trace-level"
          & metavar "0..4"
          & value 0
          & help "Set the tracing level for the XML parser"
          )
    <*> argument str (metavar "FILE")

parseCheckpoint :: Parser Command
parseCheckpoint = pure Checkpoint

parseServe :: Parser Command
parseServe = Serve
    <$> arguments (`lookup` services)
          ( metavar "dict|irc|state|web..."
          & value [minBound..]
          )
  where
    services = [("dict",DICT), ("irc",IRC), ("state",State), ("web",Web)]

parseLookup :: Parser Command
parseLookup = Lookup
    <$> nullOption
          ( reader ((`HashMap.lookup` languageTags) . fromString)
          & long "language"
          & metavar "TAG"
          & value (English UnitedStates)
          )
    <*> arguments (Just . fromString) (metavar "WORD...")

parseSearch :: Parser Command
parseSearch = Search
    <$> nullOption
          ( reader ((`HashMap.lookup` languageTags) . fromString)
          & long "language"
          & metavar "TAG"
          & value (English UnitedStates)
          )
    <*> arguments (Just . fromString) (metavar "KEYWORD...")

-- * Printing output
-- ***************************************************************************

-- | How to print output on the console.
data OutputMode = TTY      -- ^ 'Colored' if output is a terminal, otherwise 'Plain'.
                | Colored  -- ^ Print output with all formatting preserved.
                | Plain    -- ^ Strip formatting from output.
                | Quiet    -- ^ Don't output anything.

-- | Document printer that is aware of the 'OutputMode' and the width of
-- the terminal if available.
output :: (Has OutputMode m, MonadIO m) => Doc -> m ()
output doc = do
#if WINDOWS
    let width = Nothing
#else
    width <- io $ fmap (`getCapability` termColumns) setupTermFromEnv
#endif
    let prettyPrint = io . displayIO stdout
                         . renderPretty 1.0 (fromMaybe 80 width)
                         . (<> linebreak)
    mode <- fetch
    case mode of
      Colored -> prettyPrint doc
      Plain   -> prettyPrint $ plain doc
      Quiet   -> return ()
      TTY     ->
        do tty <- io $ hIsTerminalDevice stdout
           prettyPrint $ if tty then doc else plain doc


-- * Programs
-- ***************************************************************************

-- | Runtime execution environment for @kibr@ commands.
data Runtime = Runtime
    { config  :: Config
    , options :: Options
    , state   :: AcidState AppState
    }

-- | Monad transformer for monads with access to a 'Runtime' environment.
newtype ProgramT m a = Program (ReaderT Runtime m a)
                       deriving (Functor, Applicative, Monad, MonadReader Runtime, MonadIO)

-- | Programs usually run in the 'IO' monad and produce no value.
type Program = ProgramT IO ()

-- | Run a 'ProgramT' with a 'Runtime' environment and return the inner
-- monad computation.
runProgramT :: Monad m => ProgramT m a -> Runtime -> m a
runProgramT (Program m) = runReaderT m

instance Monad m => Has (AcidState AppState) (ProgramT m) where
    fetch = asks state

instance Monad m => Has OutputMode (ProgramT m) where
    fetch = asks (outputMode . options)


-- * Utilities
-- ***************************************************************************

-- | Convenient alias for 'liftIO'.
io :: MonadIO m => IO a -> m a
io = liftIO
