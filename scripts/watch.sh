#!/bin/bash

WATCH() {
  local pid
  make http &
  pid=$!
  find src -name '*.hs' | inotifywait --fromfile=- -e modify
  kill $pid
}

WATCH
