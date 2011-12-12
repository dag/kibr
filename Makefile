all: hlint test

.PHONY: build
build:
	@cabal-dev install

.PHONY: pkg-list
pkg-list:
	@ghc-pkg -f cabal-dev/packages-*.conf list

.PHONY: test
test: build
	@cabal-dev/bin/kibr --test

.PHONY: hlint
hlint:
	@hlint --color src

.PHONY: import
import:
	@cabal-dev/bin/kibr --import fixtures.xml

.PHONY: serve
serve:
	@echo "Launching server on http://localhost:8000/"
	@cabal-dev/bin/kibr
