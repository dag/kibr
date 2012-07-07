{-# LANGUAGE RecordWildCards #-}

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

import Config.Dyre                    (Params(..), wrapMain, defaultParams)
import Control.Category               ((.))
import Happstack.Server               (Conf, nullConf)
import Kibr.Data                      (fromList)
import Network                        (PortID(..))
import Network.IRC.Bot                (BotConf(..), User(..), nullBotConf, nullUser)
import Options.Applicative
import System.Environment             (getEnv)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.Exit                    (exitFailure)
import System.FilePath                ((</>))
import System.IO                      (hPutStr, stderr)

-- | Application config.
data Config = Config
    { stateDirectory :: IO FilePath  -- ^ Path to /acid-state/ log directory.
    , acidServerPort :: IO PortID    -- ^ Run an /acid-state/ server on this port.
    , webServer      :: Conf         -- ^ Configuration for the web server.
    , ircBots        :: [BotConf]    -- ^ IRC bots to run.
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
      , acidServerPort = do dir <- getEnv "XDG_RUNTIME_DIR"
                            return $ UnixSocket (dir </> "kibr" </> "acid-state.socket")
      , webServer      = cfg
      , ircBots        = [cfg {nick = "kibr", host = "chat.freenode.net",
                               commandPrefix = "@", channels = fromList ["#sampla"],
                               user = cfg {username = "kibr", realname = "Lojban IRC bot"}}]
      }

-- | The @kibr@ executable.
kibr :: Config -> IO ()
kibr cfg = run $ Right cfg

-- | An error message from GHC if /dyre/ fails to compile a user configuration.
type CompilationError = String

-- | Wrapper around 'main' handling compilation of user configuration.
run :: Either CompilationError Config -> IO ()
run = wrapMain $ defaultParams
    { projectName = "kibr"
    , showError   = const Left
    , realMain    = main
    }

data Options = Options
    { remote :: Bool
    , cmd :: Command
    }

data Command = Import FilePath | Serve [Service]

data Service = DICT | IRC | State | Web deriving (Bounded, Enum)

service :: String -> Maybe Service
service "dict"  = Just DICT
service "irc"   = Just IRC
service "state" = Just State
service "web"   = Just Web
service _       = Nothing

options :: Parser Options
options = Options
    <$> switch (long "remote" . help "Connect to remote state service")
    <*> subparser
          ( command "import" (info (helper <*> import') (progDesc "Import words from an XML export"))
          . command "serve"  (info (helper <*> serve)   (progDesc "Launch Internet services"))
          )
  where
    import' = Import <$> argument str (metavar "FILE")
    serve   = Serve  <$> arguments service (metavar "SERVICE" . value [minBound..])

-- | The actual entry-point for the @kibr@ executable.
main :: Either CompilationError Config -> IO ()
main (Left msg) = hPutStr stderr msg >> exitFailure
main (Right config@Config{..}) = do
    opts <- execParser parser
    return ()
  where
    parser = info (helper <*> options) fullDesc
