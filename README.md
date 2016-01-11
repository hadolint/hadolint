# Dockerfile Linter written in Haskell [![Build Status](https://travis-ci.org/lukasmartinelli/hadolint.svg)](https://travis-ci.org/lukasmartinelli/hadolint)

<img align="right" alt="pipecat" width="150" src="http://hadolint.lukasmartinelli.ch/img/cat_container.png" />

Try it out online: http://hadolint.lukasmartinelli.ch/

A smarter Dockerfile linter that helps you build [best practice Docker images](https://docs.docker.com/engine/articles/dockerfile_best-practices/).
The linter is parsing the Dockerfile into an AST and performs rules on top of the AST.
It additionally is using the famous [Shellcheck](https://github.com/koalaman/shellcheck) to lint the Bash
code inside `RUN` instructions.

[![Screenshot](screenshot.png)](http://hadolint.lukasmartinelli.ch/)

## How to use

**On the web**

The best way to use `hadolint` is using it online on http://hadolint.lukasmartinelli.ch/.

**From your terminal**

If you have `hadolint` installed locally you can run it on any local Dockerfile
to get the lint results.

```
hadolint <dockerfile>
```

**With Docker**

Docker comes to the rescue to provide an easy way how to run `hadolint` on most platforms.
To lint a Dockerfile you mount it into the running container and run `hadolint` on top of it.

```
docker run --rm -v $(pwd):/lint lukasmartinelli/hadolint hadolint /lint/Dockerfile
```

## Install

To install `hadolint` locally you need [Haskell](https://www.haskell.org/platform/) and [Cabal](https://wiki.haskell.org/Cabal-Install) installed.
On systems with Cabal you can now install the linter directly from Hackage (installs to `~/.cabal/bin`).

```
cabal update
cabal install hadolint
```

### Binaries

Haskell does not have such a great cross compile story like Go. If someone has experience in
creating static Haskell binaries for Windows, OSX and Linux I would be thankful for your help.

## Checks

List of implemented checks. Take a look into `Analyzer.hs` to find the implementation of the rules.

|  Rule                |  Decscription
| -------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|  HasMaintainer       |  Specify a maintainer of the Dockerfile                                                                                                                                 |
| MaintainerAddress    |  Provide an email adress or URL as maintainer                                                                                                                           |
| AbsoluteWorkdir      |  Use absolute WORKDIR                                                                                                                                                   |
| WgetOrCurl           |  Either use Wget or Curl but not both                                                                                                                                   |
| InvalidCmd           |  For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig` |
| NoRoot               |  Do not switch to root USER                                                                                                                                             |
| NoCd                 |  Use WORKDIR to switch to a directory                                                                                                                                   |
| NoSudo               |  Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root.                                                                           |
| NoUpgrade            |  Do not use apt-get upgrade or dist-upgrade.                                                                                                                            |
| NoLatestTag          |  Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag.                                                           |
| NoUntagged           |  Always tag the version of an image explicitely.                                                                                                                        |
| AptGetVersionPinning |  Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get install <package>=<version>`                                                      |
| AptGetCleanup        |  Delete the apt-get lists after installing something                                                                                                                    |
| UseAdd               |  Use ADD for extracting archives into an image                                                                                                                          |
| PipVersionPinned     |  Pin versions in pip. Instead of `pip install <package>` use `pip install <package>==<version>`                                                                         |
| InvalidPort          |  Valid UNIX ports range from 0 to 65535                                                                                                                                 |
| AptGetNoRecommends   |  Avoid additional packages by specifying `--no-install-recommends`                                                                                                        |
| AptGetYes            |  Use the -y switch: apt-get -y install <package>                                                                                                                        |

## Develop

This is my first Haskell program. If you are a experienced Haskeller I would be really thankful
if you would tear my code apart in a review.

### Setup

1. Clone repository
    ```
    git clone --recursive git@github.com:lukasmartinelli/hadolint.git
    ```
2. Create a new sandbox.
    ```
    cabal init
    ```
3. Install modified ShellCheck version
    ```
    cabal install deps/shellcheck
    ```
4. Install the dependencies
    ```
    cabal install --only-dependencies
    ```

### REPL

The easiest way to try out the parser is using the REPL.

```
cabal repl
```

In the REPL you can load the parser code with `:l Parser.hs` and use `parseString` or `parseFile` to get a quick look at the AST.

```
parseString "FROM debian:jessie"
```

### Parsing

The Dockerfile is parsed using [Parsec](https://wiki.haskell.org/Parsec) and is using the lexer `Lexer.hs` and parser `Parser.hs`.

### AST

Dockerfile syntax is is fully described in the [Dockerfile reference](http://docs.docker.com/engine/reference/builder/).  Just take a look at `Syntax.hs` to see the AST definition.


## Alternatives

- https://github.com/RedCoolBeans/dockerlint/
- https://github.com/projectatomic/dockerfile_lint/
- http://dockerfile-linter.com/
