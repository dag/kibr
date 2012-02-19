{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.XHtmlCombinators.Happstack where

import Preamble

import Control.Monad.Identity
import Data.Foldable
import Happstack.Server
import Text.XHtml.Strict              (docType, stringToHtmlString)
import Text.XHtmlCombinators.Internal

import qualified Data.Sequence as Seq
import qualified Data.Text     as T

instance ToMessage (XHtml Root) where
  toContentType _ = "application/xhtml+xml; charset=UTF-8"
  toMessage =
      toMessage
        . T.append xmlDecl
        . T.append (T.pack docType)
        . render safe
    where
      xmlDecl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

safe :: Escaper Text
safe =
    unsafe { escapeAttr = \(Attr name value) -> Attr name (escapeHtml value)
           , escapeText = escapeHtml
           , childEscaper = const safe
           }
  where
    escapeHtml = T.pack . stringToHtmlString . T.unpack


-- Below code mostly copied from Text.XHtmlCombinators.Render
-- (c) Alasdair Armstrong 2010

data Escaper e = Escaper
    { escapeAttr :: Attr -> Attr
    , escapeText :: Text -> Text
    , childEscaper :: Text -> Escaper e
    , encoder :: Text -> e
    }

unsafe :: Escaper Text
unsafe = Escaper
    { escapeAttr = id
    , escapeText = id
    , childEscaper = const unsafe
    , encoder = id
    }

lt = T.singleton '<'
gt = T.singleton '>'
space = T.singleton ' '
lts = T.pack "</"
nl = T.singleton '\n'

renderAttrs :: Escaper e -> Attrs -> Text
renderAttrs esc [] = T.empty
renderAttrs esc attrs =
    T.intercalate space $ space : fmap (renderAttr . escapeAttr esc) attrs

renderAttr :: Attr -> Text
renderAttr (Attr key val) = T.concat [key, "=\"", val, "\""]

renderNode :: Escaper e -> Node -> Text
renderNode esc (TextNode t) = t
renderNode esc (Node name rattrs attrs c)
    | Seq.null c = T.concat [lt, name, a, b, gt, lts, name, gt]
    | otherwise = T.concat
        [ lt, name, a, b, gt
        , fold (renderNode (childEscaper esc name) <$> c)
        , lts, name, gt
        ]
  where
    a = renderAttrs esc rattrs
    b = renderAttrs esc attrs

renderT :: (Functor t, Monad t, Content c) => Escaper e -> XHtmlT t c -> t e
renderT esc page = do
    content <- execXHtmlMT page
    let txt = fold (renderNode esc . toContent <$> content)
    return (encoder esc txt)

render :: Content c => Escaper e -> XHtml c -> e
render esc = runIdentity . renderT esc
