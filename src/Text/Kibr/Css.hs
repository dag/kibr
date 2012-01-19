module Text.Kibr.Css where

import Preamble

import Language.CSS
import Language.CSS.Extra
import Language.CSS.YUI

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
