all: hlint test

build:
	@cabal-dev install

test: build
	@cabal-dev/bin/kibr --test

hlint:
	@hlint --color Kibr/ kibr.hs

import:
	@cabal-dev/bin/kibr --import fixtures.xml

serve:
	@echo "Launching server on http://localhost:8000/"
	@cabal-dev/bin/kibr
