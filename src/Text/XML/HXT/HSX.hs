{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, OverlappingInstances, TypeOperators, UndecidableInstances #-}

-- | Support for using @trhsx@ together with @HXT@.  Not compatible with
-- @HSX.XMLGenerator@ or @HSP@ within individual modules.
module Text.XML.HXT.HSX
    ( genElement
    , genEElement
    , Attr((:=))
    , EmbedAsAttr(asAttr)
    , EmbedAsChild(asChild)
    )
  where

import Text.XML.HXT.Core

data Attr k v = k := v

toQName :: (Maybe String, String) -> QName
toQName (Just ns, n) = mkQName ns n ""
toQName (Nothing, n) = mkQName "" n ""

genElement :: ArrowXml (~>)
           => (Maybe String, String)
           -> [n ~> XmlTree]
           -> [n ~> XmlTree]
           -> n ~> XmlTree
genElement = mkqelem . toQName

genEElement :: ArrowXml (~>)
            => (Maybe String, String)
            -> [n ~> XmlTree]
            -> n ~> XmlTree
genEElement n a = genElement n a []

class ArrowXml (~>) => EmbedAsAttr (~>) a | a -> (~>) where
    asAttr :: a -> XmlTree ~> XmlTree

instance (ArrowXml (~>), Show v) => EmbedAsAttr (~>) (Attr String v) where
    asAttr (k := v) = sattr k (show v)

instance ArrowXml (~>) => EmbedAsAttr (~>) (Attr String String) where
    asAttr (k := v) = sattr k v

class ArrowXml (~>) => EmbedAsChild (~>) c | c -> (~>) where
    asChild :: c -> XmlTree ~> XmlTree

instance ArrowXml (~>) => EmbedAsChild (~>) (XmlTree ~> XmlTree) where
    asChild = id

instance ArrowXml (~>) => EmbedAsChild (~>) XmlTree where
    asChild = constA

instance ArrowXml (~>) => EmbedAsChild (~>) (XmlTree ~> String) where
    asChild = (>>> mkText)

instance ArrowXml (~>) => EmbedAsChild (~>) String where
    asChild = txt
