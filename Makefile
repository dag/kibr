KIBR = dist/build/kibr/kibr

all: hlint test http

cabal-dev:
	@cabal-dev install -fdevelopment --disable-optimization

.PHONY: build
build: cabal-dev
	@cabal-dev build

.PHONY: test
test: build
	@$(KIBR) test -a 1000

.PHONY: hlint
hlint:
	@hlint --color src

state:
	@$(KIBR) import fixtures.xml

.PHONY: http
http: state
	@echo "Launching server on http://localhost:8000/"
	@$(KIBR) http

.PHONY: irc
irc: state
	@$(KIBR) irc

.PHONY: gvim
gvim:
	@gvim kibr.cabal "+cd src"
