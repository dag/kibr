{-# LANGUAGE RecordWildCards #-}

-- | Support for configuration via dynamic recompilation using
-- "Config.Dyre".
--
-- For example, to configure @kibr@ to run the web application on port
-- 3000, write the following to the file @~\/.config\/kibr\/kibr.hs@:
--
-- >import Data.Conf
-- >import Happstack.Server
-- >import Kibr.Run
-- >main = kibr conf{webServer = conf{port = 3000}}
--
-- If you delete your configuration, you may need to wipe @~\/.cache\/kibr@
-- as well.

module Kibr.Run
    ( kibr
    , CompilationError
    , main
    )
  where

import Config.Dyre         (Params(..), wrapMain, defaultParams)
import Data.Acid           (openLocalStateFrom)
import Data.Acid.Remote    (openRemoteState)
import Data.Default        (def)
import Kibr.CLI
import Options.Applicative ((<*>), execParser, info, helper, fullDesc)
import System.Exit         (exitFailure)
import System.IO           (hPutStr, stderr)

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
    options@Options{..} <- execParser parser
    state <- openAcidState remote
    runProgramT program Runtime{config = config, options = options, state = state}
  where
    parser = info (helper <*> parseOptions) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def
