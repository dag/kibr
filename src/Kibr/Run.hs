{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

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
    ( Config(..)
    , kibr
    )
  where

import Prelude hiding ((.))
import Kibr.Data hiding (User)

import qualified Data.HashMap.Lazy as HashMap

import Config.Dyre                    (Params(..), wrapMain, defaultParams)
import Control.Category               ((.))
import Control.Exception              (bracket)
import Control.Monad                  (forM_, when)
import Control.Monad.Reader           (ReaderT, runReaderT, asks)
import Control.Monad.Trans            (liftIO)
import Data.Acid                      (AcidState, openLocalStateFrom, closeAcidState, createCheckpoint)
import Data.Acid.Remote               (acidServer, openRemoteState)
import Data.Configurable              (Configurable(conf))
import Data.Default                   (def)
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
import System.IO                      (hPutStr, stderr)
import Text.InterpolatedString.Perl6  (qq)
import Text.PrettyPrint.ANSI.Leijen   (Doc, (<>), plain, linebreak)
import Text.XML.HXT.Core              ((/>), runX, readDocument, withTrace)
import Text.XML.HXT.Expat             (withExpat)
import Text.XML.HXT.HTTP              (withHTTP)

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

-- | An error message from GHC if /dyre/ fails to compile a user configuration.
type CompilationError = String

data Options = Options
    { remote :: Bool
    , outputMode :: OutputMode
    , cmd :: Command
    }

data OutputMode = Colored | Plain | Quiet

type TraceLevel = Int

data Command = Import TraceLevel FilePath
             | Checkpoint
             | Serve [Service]
             | Lookup Language [Word]

data Service = DICT | IRC | State | Web deriving (Eq, Bounded, Enum)

optparser :: Parser Options
optparser = Options
    <$> switch (long "remote" . help "Connect to remote state service")
    <*> nullOption (reader outputMode . long "output" . metavar "MODE" . value Colored . help "Control how output is printed (colored|plain|quiet)")
    <*> subparser
          ( mkcmd "import" import' "Import words from an XML export"
          . mkcmd "checkpoint" checkpoint "Create a state checkpoint"
          . mkcmd "serve"  serve   "Launch Internet services"
          . mkcmd "lookup" lookup' "Look up words"
          )
  where
    mkcmd name parser desc = command name $ info (helper <*> parser) $ progDesc desc
    outputMode s = lookup s [("colored",Colored), ("plain",Plain), ("quiet",Quiet)]
    service s  = lookup s [("dict",DICT), ("irc",IRC), ("state",State), ("web",Web)]
    word       = Just . fromString
    language   = (`HashMap.lookup` languageTags) . fromString
    import'    = Import <$> option (long "trace-level" . metavar "0..4" . value 0 . help "Set the tracing level for the XML parser")
                        <*> argument str (metavar "FILE")
    checkpoint = pure Checkpoint
    serve      = Serve  <$> arguments service (metavar "dict|irc|state|web..." . value [minBound..])
    lookup'    = Lookup <$> nullOption (reader language . long "language" . metavar "TAG" . value (English UnitedStates))
                        <*> arguments word (metavar "WORD...")

data Runtime = Runtime
    { config :: Config
    , options :: Options
    , state  :: AcidState AppState
    }

-- | The actual entry-point for the @kibr@ executable.
main :: Either CompilationError Config -> IO ()
main (Left msg) = hPutStr stderr msg >> exitFailure
main (Right config@Config{..}) = do
    options@Options{..} <- execParser parser
    bracket (openAcidState remote) closeAcidState $ \state ->
      runReaderT (run cmd) Runtime {config = config, options = options, state = state}
  where
    parser = info (helper <*> optparser) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def

instance Monad m => HasAcidState (ReaderT Runtime m) AppState where
    getAcidState = asks state

output :: Doc -> ReaderT Runtime IO ()
output doc = do
    mode <- asks (outputMode . options)
    case mode of
      Colored -> liftIO $ prettyPrint (doc <> linebreak)
      Plain -> liftIO $ prettyPrint (plain doc <> linebreak)
      Quiet -> return ()

run :: Command -> ReaderT Runtime IO ()

run (Serve services) = do
    state <- asks state
    Config{..} <- asks config
    when (State `elem` services) $ liftIO $
      do (_,port) <- stateServer
         acidServer state port

run (Import traceLevel doc) = do
    dict <- liftIO $ runX $ readDocument sys doc /> readDictionary
    output "Importing..."
    update_ $ ImportWords dict
    let total = sum $ map (length . snd) dict
    output [qq|Finished importing $total words.|]
  where
    sys = [withHTTP [], withExpat True, withTrace traceLevel]

run Checkpoint = liftIO . createCheckpoint =<< asks state

run (Lookup language words) =
    forM_ words $ \word ->
      do Just typ <- query $ LookupWordType word
         Just def <- query $ LookupWordDefinition word language
         output $ linebreak <> ppWord word typ def
