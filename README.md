# Dockerfile Linter written in Haskell

There are a few existing Dockerfile linters out there where none has any real adoption. Haskell is the ideal language for writing a Dockerfile linter because it makes parsing so easy and allows integrating [Shellcheck](https://github.com/koalaman/shellcheck) at a later stage.

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
