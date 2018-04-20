# Hadolint Integrations

## Travis CI

Integration with Travis CI requires minimal changes and adding less than
two seconds to your build time.

```yaml
# Use container-based infrastructure for quicker build start-up
sudo: false
# Use generic image to cut start-up time
language: generic
env:
  # Path to 'hadolint' binary
  HADOLINT: "${HOME}/hadolint"
install:
  # Download hadolint binary and set it as executable
  - curl -sL -o ${HADOLINT} "https://github.com/hadolint/hadolint/releases/download/v1.5.0/hadolint-$(uname -s)-$(uname -m)"
    && chmod 700 ${HADOLINT}
script:
  # List files which name starts with 'Dockerfile'
  # eg. Dockerfile, Dockerfile.build, etc.
  - git ls-files --exclude='Dockerfile*' --ignored | xargs --max-lines=1 ${HADOLINT}
```

## Gitlab CI

Add the following job to your projects `.gitlab-ci.yml`:

```yaml
lint_dockerfile:
  stage: lint
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker run --rm -i hadolint/hadolint < Dockerfile
```

## Editors

Using hadolint in your terminal is not always the most convinient way, but it
can be integrated into your editor to give you a feedback as you write your
Dockerfile.

### Atom

> Atom is a text editor that's modern, approachable, yet hackable to the core—a
> tool you can customize to do anything but also use productively without ever
> touching a config file.

Thanks to [lucasdf][], there is an integration [linter-hadolint][] with
[Atom][].

![linter-hadolint-img][]

### Sublime Text 3

> A sophisticated text editor for code, markup and prose.

Thanks to [niksite][], there is an integration
[SublimeLinter-contrib-hadolint][] with [Sublime Text][].

### Vim and NeoVim

Hadolint is used in two plugins:

-   [Syntastic][] - syntax checking plugin for Vim created by Martin Grenfell.

-   [ALE][] (Asynchronous Lint Engine) - plugin for providing linting in NeoVim
    and Vim 8 while you edit your text files.

[linter-hadolint]: https://atom.io/packages/linter-hadolint
[linter-hadolint-img]: https://user-images.githubusercontent.com/18702153/33764234-7abc1f24-dc0b-11e7-96b6-4f08207b6950.png
[lucasdf]: https://github.com/lucasdf
[atom]: https://atom.io/
[sublimelinter-contrib-hadolint]: https://github.com/niksite/SublimeLinter-contrib-hadolint
[sublime text]: http://www.sublimetext.com/
[niksite]: https://github.com/niksite
[syntastic]: https://github.com/vim-syntastic/syntastic
[ale]: https://github.com/w0rp/ale
[vscode-hadolint]: https://marketplace.visualstudio.com/items?itemName=exiasr.hadolint
[vscode-hadolint-gif]: https://i.gyazo.com/a701460ccdda13a1a449b2c3e8da40bc.gif
[vs code]: https://code.visualstudio.com/
[exiasr]: https://github.com/ExiaSR

### VS Code

> Visual Studio Code is a lightweight but powerful source code editor which 
> runs on your desktop and is available for Windows, macOS and Linux. 

There is an integration [vscode-hadolint][] with [VS Code][], built by [ExiaSR][].

![vscode-hadolint-gif][]
