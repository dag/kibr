{-# LANGUAGE CPP, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, RecordWildCards #-}

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
    , run
    , CompilationError
    , main
    )
  where

import Prelude hiding ((.), catch)

import Config.Dyre                   (Params(..), wrapMain, defaultParams)
import Control.Category              ((.))
import Control.Concurrent            (forkIO, killThread)
import Control.Exception             (bracket, catch, throw)
import Control.Monad                 (forM_, when)
import Control.Monad.Reader          (asks)
import Data.Acid                     (AcidState, openLocalStateFrom, createCheckpoint)
import Data.Acid.Remote              (acidServer, openRemoteState)
import Data.Default                  (def)
import Happstack.Server.SimpleHTTP   (waitForTermination)
import Kibr.CLI
import Kibr.State
import Kibr.Text                     (ppWord)
import Kibr.XML                      (readDictionary)
import Network                       (PortID(UnixSocket))
import Options.Applicative           ((<*>), execParser, info, helper, fullDesc)
import System.Directory              (removeFile)
import System.Exit                   (exitFailure)
import System.IO                     (hPutStr, stderr)
import System.IO.Error               (isDoesNotExistError)
import Text.InterpolatedString.Perl6 (qq)
import Text.PrettyPrint.ANSI.Leijen  ((<>), linebreak)
import Text.XML.HXT.Core             ((/>), runX, readDocument, withTrace)
import Text.XML.HXT.HTTP             (withHTTP)

#ifndef WINDOWS
import Text.XML.HXT.Expat            (withExpat)
#endif

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
    runProgramT (run cmd) Runtime{config = config, options = options, state = state}
  where
    parser = info (helper <*> parseOptions) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def

-- | Dispatches the commands.
run :: Command -> Program

run (Serve services) = do
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
    ignoreNotExists e = if isDoesNotExistError e then return () else throw e

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
