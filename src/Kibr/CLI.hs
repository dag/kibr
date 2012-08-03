{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- | Command-line interface support code.
module Kibr.CLI
    ( -- * Configuration
      Config(..)
    , getRuntimeDir
      -- * Command-line options
    , Options(..)
    , OutputMode(..)
    , Command(..)
    , TraceLevel
    , Service(..)
      -- * Programs
    , Runtime(..)
    , ProgramT
    , Program
    , runProgramT
      -- * Utilities
    , io
      -- ** Output
    , HasOutputMode(..)
    , output
    )
  where

import Prelude   hiding (catch)
import Kibr.Data hiding (User)

import Control.Applicative            (Applicative, (<$>))
import Control.Monad.Reader           (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Trans            (MonadIO, liftIO)
import Data.Acid                      (AcidState)
import Data.Maybe                     (fromMaybe)
import Data.Text                      (Text)
import Happstack.Server               (Conf)
import Kibr.State
import Network                        (HostName, PortID(..))
import Network.IRC.Bot                (BotConf(..))
import System.Environment             (getEnvironment)
import System.Environment.XDG.BaseDir (getUserCacheDir)
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
    , webServer      :: Conf                  -- ^ Configuration for the web server.
    , ircBots        :: [BotConf]             -- ^ IRC bots to run.
    }

-- | Get the directory for runtime files such as sockets.
getRuntimeDir :: FilePath -> IO FilePath
getRuntimeDir dir = do
    xdg <- lookup "XDG_RUNTIME_DIR" <$> getEnvironment
    maybe (getUserCacheDir dir) (return . (</> dir)) xdg


-- * Command-line options
-- ***************************************************************************

-- | Command-line options.
data Options = Options
    { language   :: Language
    , outputMode :: OutputMode
    , cmd        :: Command
    }

-- | How to print output on the console.
data OutputMode = TTY      -- ^ 'Colored' if output is a terminal, otherwise 'Plain'.
                | Colored  -- ^ Print output with all formatting preserved.
                | Plain    -- ^ Strip formatting from output.
                | Quiet    -- ^ Don't output anything.
                deriving (Bounded, Enum, Show)

-- | The sub-commands of the @kibr@ executable, and their individual
-- options.
data Command = Import TraceLevel FilePath
             | Checkpoint
             | Serve [Service]
             | Lookup [Word]
             | Search [Text]

-- | The level of debug output to print from the XML parser.
type TraceLevel = Int

-- | Services that can be launched with @kibr serve@.
data Service = DICT | IRC | State | Web deriving (Eq, Bounded, Enum, Show)


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

instance Monad m => HasAcidState (ProgramT m) AppState where
    getAcidState = asks state

instance Monad m => HasOutputMode (ProgramT m) where
    getOutputMode = asks (outputMode . options)


-- * Utilities
-- ***************************************************************************

-- | Convenient alias for 'liftIO'.
io :: MonadIO m => IO a -> m a
io = liftIO

class HasOutputMode m where
    getOutputMode :: m OutputMode

-- | Document printer that is aware of the 'OutputMode' and the width of
-- the terminal if available.
output :: (HasOutputMode m, MonadIO m) => Doc -> m ()
output doc = do
#if WINDOWS
    let width = Nothing
#else
    width <- io $ fmap (`getCapability` termColumns) setupTermFromEnv
#endif
    let prettyPrint = io . displayIO stdout
                         . renderPretty 1.0 (fromMaybe 80 width)
                         . (<> linebreak)
    mode <- getOutputMode
    case mode of
      Colored -> prettyPrint doc
      Plain   -> prettyPrint $ plain doc
      Quiet   -> return ()
      TTY     ->
        do tty <- io $ hIsTerminalDevice stdout
           prettyPrint $ if tty then doc else plain doc
