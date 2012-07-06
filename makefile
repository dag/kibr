PATH := $(PWD)/cabal-dev/bin:$(PATH)

.PHONY: install
install:
	cabal-dev install \
	  --disable-optimization \
	  --enable-documentation \
	  --enable-executable-profiling \
	  --enable-library-profiling \
	  --enable-tests \
	  --haddock-hyperlink-source
