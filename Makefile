KIBR = dist/build/kibr/kibr

all: hlint test

.PHONY: hlint
hlint:
	hlint --color src

cabal-dev:
	cabal-dev install -fdevelopment --disable-optimization

.PHONY: build
build: cabal-dev
	cabal-dev build

.PHONY: test
test: build
	$(KIBR) test -a 1000

state:
	$(KIBR) import fixtures.xml

.PHONY: http
http: build state
	@echo "Launching server on http://localhost:8000/"
	$(KIBR) http

.PHONY: watch
watch:
	$(MAKE) http & \
	git ls-files | inotifywait --fromfile=- -e modify; \
	kill $$! || true
	$(MAKE) watch

.PHONY: irc
irc: build state
	$(KIBR) irc

src/tags:
	cd src && hasktags -c .

.PHONY: gvim
gvim: src/tags
	gvim kibr.cabal "+cd src"
