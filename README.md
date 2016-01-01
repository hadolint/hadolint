# Dockerfile Linter written in Haskell [![Build Status](https://travis-ci.org/lukasmartinelli/hadolint.svg)](https://travis-ci.org/lukasmartinelli/hadolint)

There are a few existing Dockerfile linters out there where none has any real adoption. Haskell is the ideal language for writing a Dockerfile linter because it makes parsing so easy and allows integrating [Shellcheck](https://github.com/koalaman/shellcheck) at a later stage.

## Checks

| Check                | Description
|----------------------|-----------------------------------
| **NoLatestTag**      | Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag.
| **NoSudo**           | Do not use `sudo` as it leas to unpredictable behavior. Use a tool like `gosu` to enforce root.
| **NoUpgrade**        | Do not use `apt-get upgrade` or `dist-upgrade`.
| **NoCd**             | Use WORKDIR to switch to a directory
| **InvalidCmd**       | For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`
| **WgetOrCurl**       | Either use Wget or Curl but not both
| **HasMaintainer**    | Specify a maintainer of the Dockerfile
| **AbsoluteWorkdir**  | Use absolute WORKDIR

## Parsing

The Dockerfile is parsed using [Parsec](https://wiki.haskell.org/Parsec) and is using the lexer `Lexer.hs` and parser `Parser.hs`.

Parser is nearly complete. There are still some problems with newlines and escape characters though.

## AST

Dockerfile syntax is is fully described in the [Dockerfile reference](http://docs.docker.com/engine/reference/builder/).
Just take a look at `Syntax.hs` to see the AST definition.

## Develop

Create a new sandbox.

```
cabal init
```

The easiest way to try out the parser is using the REPL.

```
cabal repl
```

In the REPL you can load the parser code with `:l Parser.hs` and use `parseString` or `parseFile` to get a quick look at the AST.

```
parseString "FROM debian:jessie"
```
