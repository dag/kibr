Kibr
====

Development Environment
-----------------------

### Prerequisites

#### Fedora

I have verified that these packages are enough to build the project on
Fedora 16:

```console
$ sudo yum install ghc cabal-dev hlint happy ghc-zlib-devel openssl-devel cryptopp-devel pcre-devel
```

#### Debian/Ubuntu

I have verified that these steps are enough to build the project on Ubuntu
Server 11.10:

```console
$ sudo apt-get install make ghc cabal-install hlint happy libghc-zlib-dev libssl-dev libcrypto++-dev libpcre3-dev
$ cabal update
$ export PATH="$HOME/.cabal/bin:$PATH"  # should go in ~/.bashrc too
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
