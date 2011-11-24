test:
	@cabal-dev install
	@cabal-dev/bin/kibr --test
	@echo "Launching server on http://localhost:8000/"
	@cabal-dev/bin/kibr fixtures.xml
