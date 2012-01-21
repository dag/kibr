KIBR = dist/build/kibr/kibr

all: hlint test serve

cabal-dev:
	@cabal-dev install

.PHONY: build
build: cabal-dev
	@cabal-dev build

.PHONY: test
test: build
	@$(KIBR) test

.PHONY: hlint
hlint:
	@hlint --color src

state:
	@$(KIBR) import fixtures.xml

.PHONY: serve
serve: state
	@echo "Launching server on http://localhost:8000/"
	@$(KIBR) http

.PHONY: gvim
gvim:
	@gvim kibr.cabal "+cd src"
