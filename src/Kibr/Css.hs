module Kibr.Css where

import Preamble hiding (Text)

import Data.Text.Lazy (Text)
import Language.CSS

master :: CSS Rule
master = do
  typography
  layout
  skin
  hsColour

typography :: CSS Rule
typography = do
  "*" `rule` do
    lineHeight . pxToPercent $ 20
  "body" `rule` do
    fontFamily "'Ubuntu', sans-serif"
  "pre, code, tt" `rule` do
    fontFamily "'Ubuntu Mono', monospace"
  "pre" `rule` do
    fontSize . pxToPercent $ 14
  "dt" `rule` do
    fontFamily "'Stoke', serif"
    fontSize . pxToPercent $ 26

layout :: CSS Rule
layout = do
  "body" `rule` do
    margin "auto"
    padding "2em"
    maxWidth "750px"
  "pre" `rule` do
    borderLeft "5px solid"
    overflow "hidden"
    padding ".5em"
  "dt" `rule` do
    margin "15px 0px"
  "dd" `rule` do
    paddingLeft "30px"

skin :: CSS Rule
skin = do
  "a" `rule` do
    color "#1160AC"
    textDecoration "none"
  "pre" `rule` do
    background "#fafafa"
    borderLeftColor "#eee"

hsColour :: CSS Rule
hsColour = do
  ".hs-str" `rule` do
    background "#fff0f0"
    color "#dd2200"
  ".hs-conid" `rule` do
    color "#888"
    fontWeight "bold"
  ".hs-keyglyph" `rule` do
    color "#080"

borderLeftColor :: Text -> CSS (Either Property Rule)
borderLeftColor = prop "border-left-color"

pxToPercent :: Int -> Text
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
pxToPercent _  = "100%"
