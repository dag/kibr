all: test

build:
	@cabal-dev install

test: build
	@cabal-dev/bin/kibr --test

serve:
	@echo "Launching server on http://localhost:8000/"
	@cabal-dev/bin/kibr fixtures.xml
