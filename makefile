PATH := "$(PWD)/cabal-dev/bin:$(PATH)"

.PHONY: install
install:
	cabal-dev install --disable-optimization         \
			  --enable-documentation         \
			  --enable-executable-profiling  \
			  --enable-library-profiling     \
			  --enable-tests                 \
			  --haddock-hyperlink-source

.PHONY: doc
doc:
	cabal haddock
	xdg-open dist/doc/html/kibr/index.html

.PHONY: test
test:
	cabal test --show-details=always --test-option=--color
