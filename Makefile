KIBR = dist/build/kibr/kibr

all: hlint test

cabal-dev:
	cabal-dev install -fdevelopment --disable-optimization

.PHONY: build
build: cabal-dev
	cabal-dev build

.PHONY: test
test: build
	$(KIBR) test -a 1000

.PHONY: hlint
hlint:
	hlint --color src

state:
	$(KIBR) import fixtures.xml

.PHONY: http
http: build state
	@echo "Launching server on http://localhost:8000/"
	$(KIBR) http

.PHONY: irc
irc: build state
	$(KIBR) irc

src/tags:
	cd src && hasktags -c .

.PHONY: gvim
gvim: src/tags
	gvim kibr.cabal "+cd src"

.PHONY: watch
watch:
	$(MAKE) http & \
	git ls-files | inotifywait --fromfile=- -e modify; \
	kill $$!
	$(MAKE) watch
