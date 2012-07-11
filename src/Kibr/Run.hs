{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards, QuasiQuotes #-}

-- | Support for configuration via dynamic recompilation using
-- "Config.Dyre".
--
-- For example, to configure @kibr@ to run the web application on port
-- 3000, write the following to the file @~\/.config\/kibr\/kibr.hs@:
--
-- >import Happstack.Server
-- >import Kibr.Run
-- >main = kibr cfg {webServer = cfg {port = 3000}}
--
-- If you delete your configuration, you may need to wipe @~\/.cache\/kibr@
-- as well.

module Kibr.Run
    ( Config(..)
    , Configurable(..)
    , kibr
    )
  where

import Prelude hiding ((.))
import Kibr.Data hiding (User)

import qualified Data.HashMap.Lazy as HashMap

import Config.Dyre                    (Params(..), wrapMain, defaultParams)
import Control.Category               ((.))
import Control.Exception              (bracket)
import Control.Monad                  (forM_, void, when)
import Control.Monad.Reader           (ReaderT, runReaderT, asks)
import Control.Monad.Trans            (liftIO)
import Data.Acid                      (AcidState, openLocalStateFrom, closeAcidState, createCheckpoint)
import Data.Acid.Remote               (acidServer, openRemoteState)
import Data.Default                   (def)
import Data.String                    (fromString)
import Happstack.Server               (Conf, nullConf)
import Kibr.State
import Kibr.Text
import Kibr.XML                       (readDictionary)
import Network                        (HostName, PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..), nullBotConf, nullUser)
import Options.Applicative
import Options.Applicative.Types      (Parser(NilP))
import System.Environment             (getEnv)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit                    (exitFailure)
import System.FilePath                ((</>))
import System.IO                      (hPutStr, stderr)
import Text.InterpolatedString.Perl6  (qq)
import Text.XML.HXT.Core              ((/>), runX, readDocument)
import Text.XML.HXT.Expat             (withExpat)
import Text.XML.HXT.HTTP              (withHTTP)

-- | Application config.
data Config = Config
    { stateDirectory :: IO FilePath           -- ^ Path to /acid-state/ log directory.
    , stateServer    :: IO (HostName,PortID)  -- ^ Location of remote /acid-state/ server.
    , webServer      :: Conf                  -- ^ Configuration for the web server.
    , ircBots        :: [BotConf]             -- ^ IRC bots to run.
    }

-- | Data types representing something configurable.
class Configurable c where
    -- | Default/base configuration.
    cfg :: c

instance Configurable Conf    where cfg = nullConf
instance Configurable BotConf where cfg = nullBotConf
instance Configurable User    where cfg = nullUser

instance Configurable Config where
    cfg = Config
      { stateDirectory = getUserDataDir $ "kibr" </> "state"
      , stateServer    = do dir <- getEnv "XDG_RUNTIME_DIR"
                            return ("127.0.0.1",UnixSocket (dir </> "kibr-state.socket"))
      , webServer      = cfg
      , ircBots        = [cfg {nick = "kibr", host = "chat.freenode.net",
                               commandPrefix = "@", channels = fromList ["#sampla"],
                               user = cfg {username = "kibr", realname = "Lojban IRC bot"}}]
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
    , cmd :: Command
    }

data Command = Import FilePath
             | Checkpoint
             | Serve [Service]
             | Lookup Language [Word]

data Service = DICT | IRC | State | Web deriving (Eq, Bounded, Enum)

options :: Parser Options
options = Options
    <$> switch (long "remote" . help "Connect to remote state service")
    <*> subparser
          ( mkcmd "import" import' "Import words from an XML export"
          . mkcmd "checkpoint" checkpoint "Create a state checkpoint"
          . mkcmd "serve"  serve   "Launch Internet services"
          . mkcmd "lookup" lookup' "Look up words"
          )
  where
    mkcmd name parser desc = command name $ info (helper <*> parser) $ progDesc desc
    service s  = lookup s [("dict",DICT), ("irc",IRC), ("state",State), ("web",Web)]
    word       = Just . fromString
    language   = (`HashMap.lookup` languageTags) . fromString
    import'    = Import <$> argument str (metavar "FILE")
    checkpoint = NilP Checkpoint
    serve      = Serve  <$> arguments service (metavar "dict|irc|state|web..." . value [minBound..])
    lookup'    = Lookup <$> nullOption (reader language . long "language" . metavar "TAG" . value (English UnitedStates))
                        <*> arguments word (metavar "WORD...")

data Runtime = Runtime
    { config :: Config
    , state  :: AcidState AppState
    }

-- | The actual entry-point for the @kibr@ executable.
main :: Either CompilationError Config -> IO ()
main (Left msg) = hPutStr stderr msg >> exitFailure
main (Right config@Config{..}) = do
    Options{..} <- execParser parser
    bracket (openAcidState remote) closeAcidState $ \state ->
      runReaderT (run cmd) Runtime {config = config, state = state}
  where
    parser = info (helper <*> options) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def

instance Monad m => HasAcidState (ReaderT Runtime m) AppState where
    getAcidState = asks state

run :: Command -> ReaderT Runtime IO ()

run (Serve services) = do
    state <- asks state
    Config{..} <- asks config
    when (State `elem` services) $ liftIO $
      do (_,port) <- stateServer
         acidServer state port

run (Import doc) = do
    dict <- liftIO $ runX $ readDocument [withHTTP [], withExpat True] doc /> readDictionary
    forM_ dict $ \(language,words) ->
      forM_ words $ \(word,wordType,wordDefinition) ->
        do void $ update $ SaveWordType word (Revision wordType SystemUser)
           void $ update $ SaveWordDefinition word language (Revision wordDefinition SystemUser)
           liftIO $ putStrLn [qq|Imported word $word for language $language|]

run Checkpoint = liftIO . createCheckpoint =<< asks state

run (Lookup language words) =
    forM_ words $ \word ->
      do Just typ <- query $ LookupWordType word
         Just def <- query $ LookupWordDefinition word language
         liftIO $ print (ppWord word typ def) >> putStrLn ""
