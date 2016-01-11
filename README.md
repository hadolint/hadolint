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

## Rules

List of implemented rules. Take a look into `Rules.hs` to find the implementation of the rules.
Rules with the prefix `DL` originate from **hadolint** while rules with the `SC` prefix originate
from **ShellCheck**. Click on the error code to get more detailed information.

Please [create an issue](https://github.com/lukasmartinelli/hadolint/issues/new) if you have an idea for a good rule.

| Rule                                                         | Decscription
| ------------------------------------------------------------ | --------------------------------------------------------------------------------------------------
| DL3000                                                       | Use absolute WORKDIR.
| DL3001                                                       | For some bash commands it makes no sense running them in a Docker container like ssh, vim, shutdown, service, ps, free, top, kill, mount, ifconfig.
| DL3002                                                       | Do not switch to root USER.
| DL3003                                                       | Use WORKDIR to switch to a directory.
| DL3004                                                       | Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root.
| DL3005                                                       | Do not use apt-get upgrade or dist-upgrade.
| DL3007                                                       | Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag.
| DL3006                                                       | Always tag the version of an image explicitely.
| DL3008                                                       | Pin versions in apt get install.
| DL3009                                                       | Delete the apt-get lists after installing something.
| DL3010                                                       | Use ADD for extracting archives into an image.
| DL3011                                                       | Valid UNIX ports range from 0 to 65535.
| DL3012                                                       | Provide an email adress or URL as maintainer.
| DL3013                                                       | Pin versions in pip.
| DL3014                                                       | Use the `-y` switch.
| DL3015                                                       | Avoid additional packages by specifying --no-install-recommends.
| DL4000                                                       | Specify a maintainer of the Dockerfile.
| DL4001                                                       | Either use Wget or Curl but not both.
| [SC1000](https://github.com/koalaman/shellcheck/wiki/SC1000) | `$` is not used specially and should therefore be escaped.
| [SC1001](https://github.com/koalaman/shellcheck/wiki/SC1001) | This `\c` will be a regular `'c'`  in this context.
| [SC1007](https://github.com/koalaman/shellcheck/wiki/SC1007) | Remove space after `=` if trying to assign a value (or for empty string, use `var='' ...`).
| [SC1010](https://github.com/koalaman/shellcheck/wiki/SC1010) | Use semicolon or linefeed before `done` (or quote to make it literal).
| [SC1015](https://github.com/koalaman/shellcheck/wiki/SC1015) | This is a unicode double quote. Delete and retype it.
| [SC1016](https://github.com/koalaman/shellcheck/wiki/SC1016) | This is a unicode single quote. Delete and retype it.
| [SC1018](https://github.com/koalaman/shellcheck/wiki/SC1018) | This is a unicode non-breaking space. Delete it and retype as space.
| [SC1035](https://github.com/koalaman/shellcheck/wiki/SC1035) | You need a space here
| [SC1037](https://github.com/koalaman/shellcheck/wiki/SC1037) | Braces are required for positionals over `9`, e.g. `${10}`.
| [SC1038](https://github.com/koalaman/shellcheck/wiki/SC1038) | Shells are space sensitive. Use `< <(cmd)`, not `<<(cmd)`.
| [SC1040](https://github.com/koalaman/shellcheck/wiki/SC1040) | When using `<<-`, you can only indent with tabs.
| [SC1044](https://github.com/koalaman/shellcheck/wiki/SC1044) | Couldn't find the end of the here doc.
| [SC1045](https://github.com/koalaman/shellcheck/wiki/SC1045) | It's not `foo &; bar`, just `foo & bar`.
| [SC1065](https://github.com/koalaman/shellcheck/wiki/SC1065) | Trying to declare parameters? Don't. Use `()` and refer to params as `$1`, `$2` etc.
| [SC1066](https://github.com/koalaman/shellcheck/wiki/SC1066) | Don't use $ on the left side of assignments.
| [SC1068](https://github.com/koalaman/shellcheck/wiki/SC1068) | Don't put spaces around the `=` in assignments.
| [SC1077](https://github.com/koalaman/shellcheck/wiki/SC1077) | For command expansion, the tick should slant left (\` vs ´).
| [SC1078](https://github.com/koalaman/shellcheck/wiki/SC1078) | Did you forget to close this double quoted string?
| [SC1079](https://github.com/koalaman/shellcheck/wiki/SC1079) | This is actually an end quote, but due to next char it looks suspect.
| [SC1081](https://github.com/koalaman/shellcheck/wiki/SC1081) | Scripts are case sensitive. Use `if`, not `If`.
| [SC1083](https://github.com/koalaman/shellcheck/wiki/SC1083) | This `{/}` is literal. Check expression (missing `;/\n`?) or quote it.
| [SC1086](https://github.com/koalaman/shellcheck/wiki/SC1086) | Don't use `$` on the iterator name in for loops.
| [SC1087](https://github.com/koalaman/shellcheck/wiki/SC1087) | Braces are required when expanding arrays, as in `${array[idx]}`.
| [SC1088](https://github.com/koalaman/shellcheck/wiki/SC1088) | Parsing stopped here. Invalid use of parentheses?
| [SC1089](https://github.com/koalaman/shellcheck/wiki/SC1089) | Parsing stopped here. Is this keyword correctly matched up?
| [SC1090](https://github.com/koalaman/shellcheck/wiki/SC1090) | Can't follow non-constant source. Use a directive to specify location.
| [SC1091](https://github.com/koalaman/shellcheck/wiki/SC1091) | Not following: (error message here)
| [SC1095](https://github.com/koalaman/shellcheck/wiki/SC1095) | You need a space or linefeed between the function name and body.
| [SC1097](https://github.com/koalaman/shellcheck/wiki/SC1097) | Unexpected `==`. For assignment, use `=`. For comparison, use `[/[[`.
| [SC1098](https://github.com/koalaman/shellcheck/wiki/SC1098) | Quote/escape special characters when using `eval`, e.g. `eval "a=(b)"`.
| [SC1099](https://github.com/koalaman/shellcheck/wiki/SC1099) | You need a space before the `#`.
| [SC2001](https://github.com/koalaman/shellcheck/wiki/SC2001) | See if you can use `${variable//search/replace}` instead.
| [SC2002](https://github.com/koalaman/shellcheck/wiki/SC2002) | Useless cat. Consider `cmd < file | ..` or `cmd file | ..` instead.
| [SC2003](https://github.com/koalaman/shellcheck/wiki/SC2003) | `expr` is antiquated. Consider rewriting this using `$((..))`, `${}`.
| [SC2004](https://github.com/koalaman/shellcheck/wiki/SC2004) | `$/${}` is unnecessary on arithmetic variables.
| [SC2005](https://github.com/koalaman/shellcheck/wiki/SC2005) | Useless `echo?`. Instead of echo `$(cmd)`, just use `cmd`.
| [SC2006](https://github.com/koalaman/shellcheck/wiki/SC2006) | Use `$(..)` instead of legacy \`..\`
| [SC2008](https://github.com/koalaman/shellcheck/wiki/SC2008) | `echo` doesn't read from stdin, are you sure you should be piping to it?
| [SC2009](https://github.com/koalaman/shellcheck/wiki/SC2009) | Consider using `pgrep` instead of grepping `ps` output.
| [SC2010](https://github.com/koalaman/shellcheck/wiki/SC2010) | Don't use `ls | grep`. Use a glob or a for loop with a condition to allow non-alphanumeric filenames.
| [SC2012](https://github.com/koalaman/shellcheck/wiki/SC2012) | Use `find` instead of `ls` to better handle non-alphanumeric filenames.
| [SC2013](https://github.com/koalaman/shellcheck/wiki/SC2013) | To read lines rather than words, pipe/redirect to a `while read` loop.
| [SC2014](https://github.com/koalaman/shellcheck/wiki/SC2014) | This will expand once before find runs, not per file found.
| [SC2015](https://github.com/koalaman/shellcheck/wiki/SC2015) | Note that `A && B || C` is not if-then-else. C may run when A is true.
| [SC2016](https://github.com/koalaman/shellcheck/wiki/SC2016) | Expressions don't expand in single quotes, use double quotes for that.
| [SC2017](https://github.com/koalaman/shellcheck/wiki/SC2017) | Increase precision by replacing `a/b*c` with `a*c/b`.
| [SC2017](https://github.com/koalaman/shellcheck/wiki/SC2017) | Increase precision by replacing `a/b*c` with `a*c/b`.
| [SC2020](https://github.com/koalaman/shellcheck/wiki/SC2020) | `tr` replaces sets of chars, not words (mentioned due to duplicates).
| [SC2021](https://github.com/koalaman/shellcheck/wiki/SC2021) | Don't use `[]` around ranges in `tr`, it replaces literal square brackets.
| [SC2022](https://github.com/koalaman/shellcheck/wiki/SC2022) | Note that unlike globs, `o*` here matches `ooo` but not `oscar`
| [SC2025](https://github.com/koalaman/shellcheck/wiki/SC2025) | Make sure all escape sequences are enclosed in `[..]` to prevent line wrapping issues
| [SC2026](https://github.com/koalaman/shellcheck/wiki/SC2026) | This word is outside of quotes. Did you intend to 'nest '"'single quotes'"' instead'?
| [SC2027](https://github.com/koalaman/shellcheck/wiki/SC2027) | The surrounding quotes actually unquote this. Remove or escape them.
| [SC2028](https://github.com/koalaman/shellcheck/wiki/SC2028) | `echo` won't expand escape sequences. Consider `printf`.
| [SC2029](https://github.com/koalaman/shellcheck/wiki/SC2029) | Note that, unescaped, this expands on the client side.
| [SC2030](https://github.com/koalaman/shellcheck/wiki/SC2030) | Modification of var is local (to subshell caused by pipeline).
| [SC2031](https://github.com/koalaman/shellcheck/wiki/SC2031) | var was modified in a subshell. That change might be lost.
| [SC2032](https://github.com/koalaman/shellcheck/wiki/SC2032) | Use own script or `sh -c '..'` to run this from `su`.
| [SC2033](https://github.com/koalaman/shellcheck/wiki/SC2033) | Shell functions can't be passed to external commands.
| [SC2034](https://github.com/koalaman/shellcheck/wiki/SC2034) | foo appears unused. Verify it or export it.
| [SC2035](https://github.com/koalaman/shellcheck/wiki/SC2035) | Use `./*glob*` or `-- *glob*` so names with dashes won't become options.
| [SC2036](https://github.com/koalaman/shellcheck/wiki/SC2036) | If you wanted to assign the output of the pipeline, use `a=$(b | c)`.
| [SC2038](https://github.com/koalaman/shellcheck/wiki/SC2038) | Use `-print0/-0` or `find -exec +` to allow for non-alphanumeric filenames.
| [SC2039](https://github.com/koalaman/shellcheck/wiki/SC2039) | In POSIX sh, something is undefined.
| [SC2041](https://github.com/koalaman/shellcheck/wiki/SC2041) | This is a literal string. To run as a command, use `$(seq 1 10)`.
| [SC2043](https://github.com/koalaman/shellcheck/wiki/SC2043) | This loop will only run once, with var=value
| [SC2044](https://github.com/koalaman/shellcheck/wiki/SC2044) | For loops over find output are fragile. Use `find -exec` or a while read loop.

### Unsupported ShellCheck Rules

Some **ShellCheck** rules collide with **hadolint** checks or make not much sense in the context of Docker
and are therefore nto supported.

| Rule                                                         | Decscription
| ------------------------------------------------------------ | --------------------------------------------------------------------------------------------------
| [SC2024](https://github.com/koalaman/shellcheck/wiki/SC2024) | `sudo` doesn't affect redirects. Use `..| sudo tee file`.

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
