module Language.CSS.YUI where

import Prelude

import Data.Text.Lazy (Text, pack)
import Text.Printf

pxToPercent :: Int -> Text
pxToPercent 10 = "77%"
pxToPercent 11 = "85%"
pxToPercent 12 = "93%"
pxToPercent 13 = "100%"
pxToPercent 14 = "108%"
pxToPercent 15 = "116%"
pxToPercent 16 = "123.1%"
pxToPercent 17 = "131%"
pxToPercent 18 = "138.5%"
pxToPercent 19 = "146.5%"
pxToPercent 20 = "153.9%"
pxToPercent 21 = "161.6%"
pxToPercent 22 = "167%"
pxToPercent 23 = "174%"
pxToPercent 24 = "182%"
pxToPercent 25 = "189%"
pxToPercent 26 = "197%"
pxToPercent px =
    pack . printf "%.1f%%" $ percentage
  where
    percentage :: Double
    percentage = fromIntegral px * (100 / 13)
