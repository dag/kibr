{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Happstack.Server.ETag where

import Preamble

import Data.Digest.Adler32
import Data.Digest.CRC32
import Happstack.Server

import qualified Data.ByteString.Char8 as B

instance Adler32 Response where
  adler32Update n = adler32Update n . rsBody

instance CRC32 Response where
  crc32Update n = crc32Update n . rsBody

setETag :: (WebMonad Response m, ServerMonad m, FilterMonad Response m)
        => String -> m ()
setETag etag =
  do
    oldETag <- getHeaderM "If-None-Match"
    case oldETag of
      Just etag' | etag' == B.pack etag ->
        finishWith . noContentLength . result 304 $ ""
      _ -> setHeaderM "ETag" etag

eTagWithFilter :: (FilterMonad Response m, ServerMonad m)
               => (Response -> String) -> m ()
eTagWithFilter tagger =
  do
    oldETag <- getHeaderM "If-None-Match"
    composeFilter $ \response -> do
      let curETag = tagger response
      case oldETag of
        Just etag | etag == B.pack curETag ->
          noContentLength . result 304 $ ""
        _ -> setHeader "ETag" curETag response

adler32ETagFilter :: (FilterMonad Response m, ServerMonad m) => m ()
adler32ETagFilter = eTagWithFilter $ show . show . adler32

crc32ETagFilter :: (FilterMonad Response m, ServerMonad m) => m ()
crc32ETagFilter = eTagWithFilter $ show . show . crc32
