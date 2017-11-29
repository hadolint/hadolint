[![Linux/OSX Build Status][travis-img]][travis]
[![Windows Build status][appveyor-img]][appveyor]
[![GPL-3 licensed][license-img]][license]

<img align="right" alt="pipecat" width="150" src="http://hadolint.lukasmartinelli.ch/img/cat_container.png" />

# Haskell Dockerfile Linter
A smarter Dockerfile linter that helps you build [best practice Docker images](https://docs.docker.com/engine/articles/dockerfile_best-practices/).
The linter is parsing the Dockerfile into an AST and performs rules on top of the AST.
It is standing on the shoulders of [Shellcheck](https://github.com/koalaman/shellcheck) to lint the Bash
code inside `RUN` instructions.

[:globe_with_meridians: **Check the online version on hadolint.lukasmartinelli.ch**](http://hadolint.lukasmartinelli.ch/.)

[![Screenshot](screenshot.png)](http://hadolint.lukasmartinelli.ch/)

## How to use

You can run `hadolint` locally to lint your Dockerfile.

```bash
hadolint <Dockerfile>
hadolint --ignore DL3003 --ignore DL3006 <Dockerfile> # exclude specific rules
```

Docker comes to the rescue to provide an easy way how to run `hadolint` on most platforms.
Just pipe your `Dockerfile` to `docker run`:

```
docker run --rm -i hadolint/hadolint < Dockerfile
```

## Install

You can download prebuilt binaries for OSX, Windows and Linux from the latest
[release page][]. However, they may not run on your system configuration since
I am not able to provide completely statically linked binaries. Fall back to
`brew`, source installation or Docker if it doesn't work for you.

If you are on OSX you can use [brew](http://brew.sh/) to install hadolint.

```bash
brew install hadolint
```

You can also build `hadolint` locally. You need [Haskell](https://www.haskell.org/platform/) and
the [stack build tool](http://docs.haskellstack.org/en/stable/install_and_upgrade.html) to build the binary.

```bash
git clone https://github.com/hadolint/hadolint
cd hadolint
stack build
```

## Rules

An incomplete list of implemented rules. Click on the error code to get more
detailed information.

-   Rules with the prefix `DL` originate from **hadolint**. Take a look at
`Rules.hs` to find the implementation of the rules.

-   Rules with the `SC` prefix originate from **ShellCheck** (Only the most
common rules are listed, there are dozens more)

Please [create an issue][] if you have an idea for a good rule.

| Rule                                                         | Description                                                                                                                                         |
|--------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------|
| [DL3000](https://github.com/hadolint/hadolint/wiki/DL3000)   | Use absolute WORKDIR.                                                                                                                               |
| [DL3001](https://github.com/hadolint/hadolint/wiki/DL3001)   | For some bash commands it makes no sense running them in a Docker container like ssh, vim, shutdown, service, ps, free, top, kill, mount, ifconfig. |
| [DL3002](https://github.com/hadolint/hadolint/wiki/DL3002)   | Do not switch to root USER.                                                                                                                         |
| [DL3003](https://github.com/hadolint/hadolint/wiki/DL3003)   | Use WORKDIR to switch to a directory.                                                                                                               |
| [DL3004](https://github.com/hadolint/hadolint/wiki/DL3004)   | Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root.                                                        |
| [DL3005](https://github.com/hadolint/hadolint/wiki/DL3005)   | Do not use apt-get upgrade or dist-upgrade.                                                                                                         |
| [DL3007](https://github.com/hadolint/hadolint/wiki/DL3007)   | Using latest is prone to errors if the image will ever update. Pin the version explicitly to a release tag.                                         |
| [DL3006](https://github.com/hadolint/hadolint/wiki/DL3006)   | Always tag the version of an image explicitly.                                                                                                      |
| [DL3008](https://github.com/hadolint/hadolint/wiki/DL3008)   | Pin versions in apt-get install.                                                                                                                    |
| [DL3009](https://github.com/hadolint/hadolint/wiki/DL3009)   | Delete the apt-get lists after installing something.                                                                                                |
| [DL3010](https://github.com/hadolint/hadolint/wiki/DL3010)   | Use ADD for extracting archives into an image.                                                                                                      |
| [DL3011](https://github.com/hadolint/hadolint/wiki/DL3011)   | Valid UNIX ports range from 0 to 65535.                                                                                                             |
| [DL3012](https://github.com/hadolint/hadolint/wiki/DL3012)   | Provide an email address or URL as maintainer.                                                                                                      |
| [DL3013](https://github.com/hadolint/hadolint/wiki/DL3013)   | Pin versions in pip.                                                                                                                                |
| [DL3014](https://github.com/hadolint/hadolint/wiki/DL3014)   | Use the `-y` switch.                                                                                                                                |
| [DL3015](https://github.com/hadolint/hadolint/wiki/DL3015)   | Avoid additional packages by specifying --no-install-recommends.                                                                                    |
| [DL3016](https://github.com/hadolint/hadolint/wiki/DL3016)   | Pin versions in `npm`.                                                                                                                              |
| [DL3020](https://github.com/hadolint/hadolint/wiki/DL3020)   | Use `COPY` instead of `ADD` for files and folders.                                                                                                  |
| [DL4000](https://github.com/hadolint/hadolint/wiki/DL4000)   | Specify a maintainer of the Dockerfile.                                                                                                             |
| [DL4001](https://github.com/hadolint/hadolint/wiki/DL4001)   | Either use Wget or Curl but not both.                                                                                                               |
| [DL4003](https://github.com/hadolint/hadolint/wiki/DL4003)   | Multiple `CMD` instructions found.                                                                                                                  |
| [DL4004](https://github.com/hadolint/hadolint/wiki/DL4004)   | Multiple `ENTRYPOINT` instructions found.                                                                                                           |
| [DL4005](https://github.com/hadolint/hadolint/wiki/DL4005)   | Use `SHELL` to change the default shell.                                                                                                            |
| [SC1000](https://github.com/koalaman/shellcheck/wiki/SC1000) | `$` is not used specially and should therefore be escaped.                                                                                          |
| [SC1001](https://github.com/koalaman/shellcheck/wiki/SC1001) | This `\c` will be a regular `'c'`  in this context.                                                                                                 |
| [SC1007](https://github.com/koalaman/shellcheck/wiki/SC1007) | Remove space after `=` if trying to assign a value (or for empty string, use `var='' ...`).                                                         |
| [SC1010](https://github.com/koalaman/shellcheck/wiki/SC1010) | Use semicolon or linefeed before `done` (or quote to make it literal).                                                                              |
| [SC1018](https://github.com/koalaman/shellcheck/wiki/SC1018) | This is a unicode non-breaking space. Delete it and retype as space.                                                                                |
| [SC1035](https://github.com/koalaman/shellcheck/wiki/SC1035) | You need a space here                                                                                                                               |
| [SC1045](https://github.com/koalaman/shellcheck/wiki/SC1045) | It's not `foo &; bar`, just `foo & bar`.                                                                                                            |
| [SC1065](https://github.com/koalaman/shellcheck/wiki/SC1065) | Trying to declare parameters? Don't. Use `()` and refer to params as `$1`, `$2` etc.                                                                |
| [SC1066](https://github.com/koalaman/shellcheck/wiki/SC1066) | Don't use $ on the left side of assignments.                                                                                                        |
| [SC1068](https://github.com/koalaman/shellcheck/wiki/SC1068) | Don't put spaces around the `=` in assignments.                                                                                                     |
| [SC1077](https://github.com/koalaman/shellcheck/wiki/SC1077) | For command expansion, the tick should slant left (\` vs ´).                                                                                        |
| [SC1078](https://github.com/koalaman/shellcheck/wiki/SC1078) | Did you forget to close this double-quoted string?                                                                                                  |
| [SC1079](https://github.com/koalaman/shellcheck/wiki/SC1079) | This is actually an end quote, but due to next char, it looks suspect.                                                                              |
| [SC1081](https://github.com/koalaman/shellcheck/wiki/SC1081) | Scripts are case sensitive. Use `if`, not `If`.                                                                                                     |
| [SC1083](https://github.com/koalaman/shellcheck/wiki/SC1083) | This `{/}` is literal. Check expression (missing `;/\n`?) or quote it.                                                                              |
| [SC1086](https://github.com/koalaman/shellcheck/wiki/SC1086) | Don't use `$` on the iterator name in for loops.                                                                                                    |
| [SC1087](https://github.com/koalaman/shellcheck/wiki/SC1087) | Braces are required when expanding arrays, as in `${array[idx]}`.                                                                                   |
| [SC1095](https://github.com/koalaman/shellcheck/wiki/SC1095) | You need a space or linefeed between the function name and body.                                                                                    |
| [SC1097](https://github.com/koalaman/shellcheck/wiki/SC1097) | Unexpected `==`. For assignment, use `=`. For comparison, use `[/[[`.                                                                               |
| [SC1098](https://github.com/koalaman/shellcheck/wiki/SC1098) | Quote/escape special characters when using `eval`, e.g. `eval "a=(b)"`.                                                                             |
| [SC1099](https://github.com/koalaman/shellcheck/wiki/SC1099) | You need a space before the `#`.                                                                                                                    |
| [SC2002](https://github.com/koalaman/shellcheck/wiki/SC2002) | Useless cat. Consider `cmd < file | ..` or `cmd file | ..` instead.                                                                                 |
| [SC2015](https://github.com/koalaman/shellcheck/wiki/SC2015) | Note that `A && B || C` is not if-then-else. C may run when A is true.                                                                              |
| [SC2026](https://github.com/koalaman/shellcheck/wiki/SC2026) | This word is outside of quotes. Did you intend to 'nest '"'single quotes'"' instead'?                                                               |
| [SC2028](https://github.com/koalaman/shellcheck/wiki/SC2028) | `echo` won't expand escape sequences. Consider `printf`.                                                                                            |
| [SC2035](https://github.com/koalaman/shellcheck/wiki/SC2035) | Use `./*glob*` or `-- *glob*` so names with dashes won't become options.                                                                            |
| [SC2046](https://github.com/koalaman/shellcheck/wiki/SC2046) | Quote this to prevent word splitting                                                                                                                |
| [SC2086](https://github.com/koalaman/shellcheck/wiki/SC2086) | Double quote to prevent globbing and word splitting.                                                                                                |
| [SC2140](https://github.com/koalaman/shellcheck/wiki/SC2140) | Word is in the form `"A"B"C"` (B indicated). Did you mean `"ABC"` or `"A\"B\"C"`?                                                                   |
| [SC2154](https://github.com/koalaman/shellcheck/wiki/SC2154) | var is referenced but not assigned.                                                                                                                 |
| [SC2164](https://github.com/koalaman/shellcheck/wiki/SC2164) | Use `cd ... || exit` in case `cd` fails.                                                                                                            |

## Develop

This is my first Haskell program. If you are an experienced Haskeller I would be really thankful
if you would tear my code apart in a review.

### Setup

1. Clone repository
    ```bash
    git clone --recursive git@github.com:hadolint/hadolint.git
    ```
2. Install the dependencies
    ```bash
    stack install
    ```

### REPL

The easiest way to try out the parser is using the REPL.

```bash
# start the repl
stack repl
# parse instruction and look at AST representation
parseString "FROM debian:jessie"
```

### Tests

Run unit tests.

```bash
stack test
```

Run integration tests.

```
./integration_test.sh
```

### Parsing

The Dockerfile is parsed using [Parsec](https://wiki.haskell.org/Parsec) and is using the lexer `Lexer.hs` and parser `Parser.hs`.

### AST

Dockerfile syntax is fully described in the [Dockerfile reference](http://docs.docker.com/engine/reference/builder/).  Just take a look at `Syntax.hs` to see the AST definition.


## Alternatives

- https://github.com/RedCoolBeans/dockerlint/
- https://github.com/projectatomic/dockerfile_lint/
- http://dockerfile-linter.com/
<!-- References -->
[release page]: https://github.com/hadolint/hadolint/releases/latest
