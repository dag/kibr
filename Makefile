HLINT  = cabal-dev/bin/hlint
KIBR   = dist/build/kibr/kibr
TARGET = http
PORT   = 8000

all: check-cabal test

.PHONY: check-cabal
check-cabal:
	cabal check

cabal-dev:
	cabal-dev install   -fdevelopment --disable-optimization --only-dependencies --ghc-option=-XFlexibleInstances
	cabal-dev configure -fdevelopment --disable-optimization

.PHONY: build
build: cabal-dev
	cabal-dev build

.PHONY: test
test: build
	$(KIBR) test -a 1000

state:
	$(KIBR) import data/fixtures.xml

.PHONY: http
http: build state
	@echo "Launching server on http://localhost:$(PORT)/"
	$(KIBR) --http-port $(PORT) http

.PHONY: watch
watch:
	$(MAKE) $(TARGET) & \
	git ls-files | inotifywait --fromfile=- -e modify; \
	kill $$! || true
	$(MAKE) watch

.PHONY: irc
irc: build state
	$(KIBR) irc
