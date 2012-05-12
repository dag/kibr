HLINT       = cabal-dev/bin/hlint
RTS_OPTS    = -N
KIBR        = dist/build/kibr/kibr +RTS $(RTS_OPTS) -RTS
TARGET      = http
PORT        = 8000
CABAL_FLAGS = -fdevelopment --enable-library-profiling --enable-executable-profiling --disable-optimization

all: check-cabal test

.PHONY: check-cabal
check-cabal:
	cabal check

cabal-dev:
	cabal-dev install $(CABAL_FLAGS) --only-dependencies --ghc-option=-XFlexibleInstances
	cabal-dev configure $(CABAL_FLAGS)

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

.PHONY: deploy
deploy:
	cabal sdist
	scp -C dist/kibr-0.0.0.tar.gz dag@vrici.lojban.org:~/kibr/kibr-0.0.0.tar.gz
	ssh -C dag@vrici.lojban.org 'cd kibr; cabal-dev install kibr-0.0.0.tar.gz --constraint="template-haskell == 2.5.0.0" --force-reinstalls'
