Kibr
====

Development Environment
-----------------------

### Prerequisites

I'm using these versions on Fedora 16; other versions may or may not work:

* GHC 7.0.4
* Cabal 1.10.2.0
* [cabal-dev](http://hackage.haskell.org/package/cabal-dev) 0.9
* [HLint](http://community.haskell.org/~ndm/hlint/) 1.8.13

```console
$ sudo yum install ghc cabal-dev hlint
```

The Happstack server builds with support for HTTPS by default, which
requires some foreign libraries:

```console
$ sudo yum install openssl-devel cryptopp-devel
```

If your operating system has a package for cabal-install *but not*
cabal-dev and/or HLint you can install either of them this way:

```console
$ cabal update
$ cabal install cabal-dev hlint
```

Make sure to add `~/.cabal/bin` to your `$PATH`:

```bash
export PATH="$HOME/.cabal/bin:$PATH"
```

This might work on Debian derivatives:

```console
$ sudo apt-get install ghc cabal-install hlint libssl-dev libcrypto++-dev
$ cabal update
$ cabal install cabal-dev
```

### Workflow

*Note: some changes require passing `-B` to make, for example if you've
added a dependency to Cabal or edited the fixtures.*

#### Automated Verification

```console
$ make
```

1. Run HLint on the source code
2. If needed, set up a cabal-dev environment and install the dependencies
   into it
3. Build the application
4. Run the tests

#### Web Application

```console
$ make http
```

1. Build the application
2. If needed, import the fixtures to the database
3. Launch the HTTP server on [localhost:8000](http://localhost:8000/)

```console
$ make watch
```

1. Run `make http` and wait for changes to files tracked by Git
2. Kill the HTTP server and start over at step one

This requires [inotify-tools](http://inotify-tools.sourceforge.net/):

```console
$ sudo {yum,apt-get} install inotify-tools
```

#### IRC Bot

```console
$ make irc
```

1. Build the application
2. If needed, import the fixtures to the database
3. Connect to freenode and join [#sampla](irc://irc.freenode.net/sampla)
