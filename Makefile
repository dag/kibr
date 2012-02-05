KIBR   = dist/build/kibr/kibr
TARGET = http
PORT   = 8000

all: hlint test

.PHONY: hlint
hlint:
	hlint --color src

cabal-dev:
	cabal update
	cabal-dev install -fdevelopment --disable-optimization

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
	$(KIBR) http --http-port $(PORT)

.PHONY: watch
watch:
	$(MAKE) $(TARGET) & \
	git ls-files | inotifywait --fromfile=- -e modify; \
	kill $$! || true
	$(MAKE) watch

.PHONY: irc
irc: build state
	$(KIBR) irc

tags:
	hasktags -c src

.PHONY: gvim
gvim: tags
	gvim $$(git ls-files)
