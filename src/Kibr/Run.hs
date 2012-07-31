{-# LANGUAGE RecordWildCards #-}

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
    ( kibr
    , CompilationError
    , main
    )
  where

import Prelude hiding (catch)

import Config.Dyre         (Params(..), wrapMain, defaultParams)
import Control.Exception   (catch, throw)
import Control.Monad       (unless)
import Data.Acid           (openLocalStateFrom)
import Data.Acid.Remote    (openRemoteState)
import Data.Default        (def)
import Kibr.CLI
import Options.Applicative ((<*>), execParser, info, helper, fullDesc)
import System.Exit         (exitFailure)
import System.IO           (hPutStr, stderr)
import System.IO.Error     (isDoesNotExistError)

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
    state   <- openRemote `catch` tryLocal
    runProgramT program Runtime{..}
  where
    parser     = info (helper <*> parseOptions) fullDesc
    openRemote = do (host,port) <- stateServer
                    openRemoteState host port
    openLocal  = do dir <- stateDirectory
                    openLocalStateFrom dir def
    tryLocal e = do unless (isDoesNotExistError e) $ throw e
                    openLocal
