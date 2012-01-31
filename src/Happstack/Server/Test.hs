module Happstack.Server.Test where

import Preamble

import Happstack.Server                      hiding (path)
import Happstack.Server.SURI                 (toSURI, query, path)
import Happstack.Server.Internal.MessageWrap (pathEls, queryInput)
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map             as Map

mkRequest :: String -> IO Request
mkRequest uri =
  do
    ib <- newEmptyMVar
    b  <- newMVar $ Body LB.empty
    pure Request { rqMethod      = GET
                 , rqSecure      = False
                 , rqPaths       = pathEls $ path u
                 , rqUri         = path u
                 , rqQuery       = query u
                 , rqInputsQuery = queryInput u
                 , rqInputsBody  = ib
                 , rqCookies     = []
                 , rqVersion     = HttpVersion 1 1
                 , rqHeaders     = Map.empty
                 , rqBody        = b
                 , rqPeer        = ("", 0)
                 }
  where
    u = toSURI uri
