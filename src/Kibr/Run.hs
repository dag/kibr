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
    , CompilationError
    , main
    , program
    , runServe
    , runImport
    , runCheckpoint
    , runLookup
    , runSearch
    )
  where

import Prelude hiding ((.), catch)

import Config.Dyre                   (Params(..), wrapMain, defaultParams)
import Control.Category              ((.))
import Control.Concurrent            (forkIO, killThread)
import Control.Exception             (bracket, catch, throw)
import Control.Monad                 (forM_, when, unless)
import Control.Monad.Reader          (asks)
import Data.Acid                     (AcidState, openLocalStateFrom, createCheckpoint)
import Data.Acid.Remote              (acidServer, openRemoteState)
import Data.Default                  (def)
import Data.Text                     (Text)
import Happstack.Server.SimpleHTTP   (waitForTermination)
import Kibr.CLI
import Kibr.Data
import Kibr.State
import Kibr.Text                     (ppWord, ppWords)
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
    runProgramT program Runtime{config = config, options = options, state = state}
  where
    parser = info (helper <*> parseOptions) fullDesc
    openAcidState True  = do (host,port) <- stateServer
                             openRemoteState host port
    openAcidState False = do dir <- stateDirectory
                             openLocalStateFrom dir def

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
