# Hadolint Integrations

## Code Review

### Codacy

[Codacy](https://www.codacy.com/) automates hadolint code reviews on every
commit and pull request, reporting code style and error prone issues.

### Codeship Pro

Add the hadolint docker container on codeship-services.yml with a docker volume
with the repository attached to it:

```yaml
hadolint:
  image: hadolint/hadolint:latest-debian
  volumes:
    - ./:/test
```

Then add the CI step on codeship-steps.yml with the path of the dockerfile

```yaml
- type: parallel
  # optional: set branches
  tag: '^(master|develop/.*)$'
  steps:
    - service: hadolint
      command: hadolint /test/Dockerfile
```

## Continuous Integration

### Travis CI

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
  - curl -sL -o ${HADOLINT} "https://github.com/hadolint/hadolint/releases/download/v1.17.6/hadolint-$(uname -s)-$(uname -m)"
    && chmod 700 ${HADOLINT}
script:
  # List files which name starts with 'Dockerfile'
  # eg. Dockerfile, Dockerfile.build, etc.
  - git ls-files --exclude='Dockerfile*' --ignored | xargs --max-lines=1 ${HADOLINT}
```

### GitHub Actions

For GitHub Actions you can use the [Hadolint Action in the GitHub Marketplace](https://github.com/marketplace/actions/hadolint-action).

```yaml
name: Lint Dockerfile

on: push

jobs:
  linter:
    runs-on: ubuntu-latest
    steps:

      - uses: actions/checkout@v2

      - name: Lint Dockerfile
        uses: brpaz/hadolint-action@master
        with:
          dockerfile: "Dockerfile"

```

### Gitlab CI

For GitLab CI you need a basic shell in your docker image so you have to use
the debian based images of hadolint.

Add the following job to your project's `.gitlab-ci.yml`:

```yaml
lint_dockerfile:
  image: hadolint/hadolint:latest-debian
  script:
    - hadolint Dockerfile
```

### Drone CI

For Drone CI, a basic shell is similiarly required.

Add the following job to your project's `.drone.yml` pipeline (drone version 0.8 or earlier):

```yaml
  hadolint:
    group: validate
    image: hadolint/hadolint:latest-debian
    commands:
      - hadolint --version
      - hadolint Dockerfile
```

Add the following job to your project's `.drone.yml` pipeline (drone version 1.0 or later):

```yaml
  - name: hadolint
    image: hadolint/hadolint:latest-debian
    commands:
      - hadolint --version
      - hadolint  Dockerfile
```

### CircleCI

For CircleCI integration use the [docker orb](https://circleci.com/orbs/registry/orb/circleci/docker).
Update your project's `.circleci/config.yml` pipeline (workflows version 2.1),
adding the docker orb and you can call the job docker/hadolint:

```yaml
orbs:
  docker: circleci/docker@x.y.z
version: 2.1
workflows:
  lint:
    jobs:
      - docker/hadolint:
          dockerfiles: 'path/to/Dockerfile,path/to/another/Dockerfile'
          ignore-rules: 'DL4005,DL3008'
          trusted-registries: 'docker.io,my-company.com:5000'
```

### Jenkins declarative pipeline

You can add a step during your CI process to lint and archive the output of hadolint

```groovy
stage ("lint dockerfile") {
    agent {
        docker {
            image 'hadolint/hadolint:latest-debian'
        }
    }
    steps {
        sh 'hadolint dockerfiles/* | tee -a hadolint_lint.txt'
    }
    post {
        always {
            archiveArtifacts 'hadolint_lint.txt'
        }
    }
}
```

### Jenkins K8S plugin

You can add an hadolint container to pod definition:

```yaml
- name: hadolint
  image: hadolint/hadolint:latest-debian
  imagePullPolicy: Always
  command:
    - cat
  tty: true
```

Then go with the linter stage:

```groovy
stage('lint dockerfile') {
    steps {
        container('hadolint') {
            sh 'hadolint dockerfiles/* | tee -a hadolint_lint.txt'
        }
    }
    post {
        always {
            archiveArtifacts 'hadolint_lint.txt'
        }
    }
}
```

### Bitbucket Pipelines

Create a `bitbucket-pipelines.yml` configuration file:

```yaml
pipelines:
  default:
    - step:
        image: hadolint/hadolint:latest-debian
        script:
          - hadolint Dockerfile
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

### Emacs
> Emacs is an extensible, customizable, free/libre text editor.

- [flycheck][] - a modern on-the-fly syntax checking extension for GNU Emacs.

### VS Code

> Visual Studio Code is a lightweight but powerful source code editor which
> runs on your desktop and is available for Windows, macOS and Linux.

There is an integration [vscode-hadolint][] with [VS Code][], built by [ExiaSR][].

![vscode-hadolint-gif][]

### Geany

> Geany is a powerful, stable and lightweight programmer's text editor
> that provides tons of useful features without bogging down your workflow.
> It runs on Linux, Windows and MacOS is translated into over 40 languages,
> and has built-in support for more than 50 programming languages.

The following can be used as a
[build action](https://www.geany.org/manual/current/index.html#build-menu-commands-dialog)
to
[lint](https://www.geany.org/manual/current/index.html#lint) Dockerfiles.

```sh
if docker run --rm -i hadolint/hadolint < "%d/%f"
| sed -re 's|^/dev/stdin:([0-9]*)|%d/%f:\1:WARNING:|'
| grep -EC100 ':WARNING:' ; then exit 1 ; else exit 0 ; fi
```

## Other

Following section contains different angles on `hadolint` usage 
that go outside of [code review](#code-review) or [editors' integration](#editors). 

- [hadolint-shim](https://github.com/kornicameister/hadolint-shim) - use `hadolint` 
  as normal executable on systems where static binaries are not available

[linter-hadolint]: https://atom.io/packages/linter-hadolint
[linter-hadolint-img]: https://user-images.githubusercontent.com/18702153/33764234-7abc1f24-dc0b-11e7-96b6-4f08207b6950.png
[lucasdf]: https://github.com/lucasdf
[atom]: https://atom.io/
[sublimelinter-contrib-hadolint]: https://github.com/niksite/SublimeLinter-contrib-hadolint
[sublime text]: http://www.sublimetext.com/
[niksite]: https://github.com/niksite
[syntastic]: https://github.com/vim-syntastic/syntastic
[ale]: https://github.com/w0rp/ale
[flycheck]: https://www.flycheck.org/en/latest/languages.html#dockerfile
[vscode-hadolint]: https://marketplace.visualstudio.com/items?itemName=exiasr.hadolint
[vscode-hadolint-gif]: https://i.gyazo.com/a701460ccdda13a1a449b2c3e8da40bc.gif
[vs code]: https://code.visualstudio.com/
[exiasr]: https://github.com/ExiaSR
