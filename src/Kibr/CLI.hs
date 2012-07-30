{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

-- | Command-line interface support code.
module Kibr.CLI
    ( -- * Configuration
      Config(..)
    , baseConfig
    , botConfig
    , getRuntimeDir
      -- * Command-line options
    , Options(..)
    , Command(..)
    , TraceLevel
    , Service(..)
      -- ** Applicative option parsers
    , enumValues
    , enumOption
    , enumArguments
    , valueAll
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

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set          as Set

import Control.Concurrent             (forkIO, killThread)
import Control.Exception              (bracket, catch, throw)
import Control.Monad                  (forM_, when, unless)
import Control.Monad.Reader           (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Trans            (MonadIO, liftIO)
import Data.Acid                      (AcidState, createCheckpoint)
import Data.Acid.Remote               (acidServer)
import Data.Char                      (toLower)
import Data.Has                       (Has(fetch))
import Data.Maybe                     (fromMaybe)
import Data.String                    (fromString)
import Data.Text                      (Text)
import Happstack.Server               (Conf, nullConf)
import Happstack.Server.SimpleHTTP    (waitForTermination)
import Kibr.State
import Kibr.Text                      (ppWord, ppWords)
import Kibr.XML                       (readDictionary)
import Network                        (HostName, PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..), nullBotConf, nullUser)
import Options.Applicative
import System.Directory               (createDirectoryIfMissing, removeFile)
import System.Environment             (getEnvironment)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserCacheDir)
import System.FilePath                ((</>))
import System.IO                      (stdout, hIsTerminalDevice)
import System.IO.Error                (isDoesNotExistError)
import Text.InterpolatedString.Perl6  (qq)
import Text.PrettyPrint.ANSI.Leijen   (Doc, (<>), displayIO, renderPretty, plain, linebreak)
import Text.XML.HXT.Core              ((/>), runX, readDocument, withTrace)
import Text.XML.HXT.HTTP              (withHTTP)

#ifndef WINDOWS
import System.Console.Terminfo        (getCapability, termColumns, setupTermFromEnv)
import Text.XML.HXT.Expat             (withExpat)
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

-- | Get the directory for runtime files such as sockets.
getRuntimeDir :: FilePath -> IO FilePath
getRuntimeDir dir = do
    xdg <- lookup "XDG_RUNTIME_DIR" <$> getEnvironment
    maybe (getUserCacheDir dir) (return . (</> dir)) xdg


-- * Command-line options
-- ***************************************************************************

-- | Command-line options.
data Options = Options
    { remote     :: Bool
    , language   :: Language
    , outputMode :: OutputMode
    , cmd        :: Command
    }

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


-- * Applicative option parsers
-- ***************************************************************************

enumValues :: (Bounded a, Enum a, Show a) => [(String,a)]
enumValues = map ((,) =<< map toLower . show) [minBound..]

enumOption :: (Bounded a, Enum a, Show a) => Mod OptionFields a -> Parser a
enumOption mod = nullOption (reader (`lookup` enumValues) & mod)

enumArguments :: (Bounded a, Enum a, Show a) => Mod ArgumentFields [a] -> Parser [a]
enumArguments = arguments (`lookup` enumValues)

valueAll :: (Bounded a, Enum a) => Mod f [a]
valueAll = value [minBound..]

parseOptions :: Parser Options
parseOptions = Options
    <$> switch (long "remote" & help "Connect to remote state service")
    <*> nullOption
          ( reader ((`HashMap.lookup` languageTags) . fromString)
          & long "language"
          & metavar "TAG"
          & value (English UnitedStates)
          & help "Select language of dictionary"
          )
    <*> enumOption
          ( long "output"
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
parseServe = Serve <$> enumArguments (valueAll & metavar "dict|irc|state|web...")

parseLookup :: Parser Command
parseLookup = Lookup <$> arguments (Just . fromString) (metavar "WORD...")

parseSearch :: Parser Command
parseSearch = Search <$> arguments (Just . fromString) (metavar "KEYWORD...")

-- * Printing output
-- ***************************************************************************

-- | How to print output on the console.
data OutputMode = TTY      -- ^ 'Colored' if output is a terminal, otherwise 'Plain'.
                | Colored  -- ^ Print output with all formatting preserved.
                | Plain    -- ^ Strip formatting from output.
                | Quiet    -- ^ Don't output anything.
                deriving (Bounded, Enum, Show)

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
