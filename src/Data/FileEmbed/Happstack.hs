module Data.FileEmbed.Happstack where

import Preamble

import Happstack.Server                          (setHeader, toResponse)
import Happstack.Server.FileServe.BuildingBlocks (guessContentType, mimeTypes)
import Language.Haskell.TH                       (Q, Exp)

import Data.FileEmbed (embedFile)

embedFileAsResponse :: FilePath -> Q Exp
embedFileAsResponse f =
    [| setHeader "Content-Type" contentType $ toResponse $(embedFile f) |]
  where
    contentType =
      fromMaybe "application/octet-stream" $ guessContentType mimeTypes f
