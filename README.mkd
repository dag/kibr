Kibr
====

Development Environment
-----------------------

### Prerequisites

I'm using these versions on Fedora 16; other versions may or may not work fine:

* GHC 7.0.4
* Cabal 1.10.2.0
* [cabal-dev](http://hackage.haskell.org/package/cabal-dev) 0.9
* [HLint](http://community.haskell.org/~ndm/hlint/) 1.8.13

```console
$ sudo yum install ghc cabal-dev hlint
```

If your operating system has a package for cabal-install *but not*
cabal-dev and/or HLint you can install either of them this way:

```console
$ cabal update
$ cabal install cabal-dev hlint
```

The Happstack server builds with support for HTTPS by default, which
requires some foreign libraries:

```console
$ sudo yum install openssl-devel cryptopp-devel
```

This might work on Debian derivatives:

```console
$ sudo apt-get install ghc cabal-install hlint libssl-dev libcrypto++-dev
$ cabal update
$ cabal install cabal-dev
```

Make sure to add `~/.cabal/bin` to your `$PATH`:

```bash
export PATH="$HOME/.cabal/bin:$PATH"
```

### Workflow

Just run make:

```console
$ make
```

This will:

1. Run HLint on the source code
2. Set up a cabal-dev environment if one isn't already set up, and install
   the dependencies into it
3. Build the application
4. Run the tests
5. Import the fixtures into the persistent state, if not already imported
6. Launch the HTTP server on [localhost:8000](http://localhost:8000/)

Sometimes, this is not enough -- for example if a new dependency is added
or the fixtures are modified.  In that case you can simply ask make to run
all targets unconditionally:

```console
$ make -B
```
