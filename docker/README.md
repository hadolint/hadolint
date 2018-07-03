[![Docker Pulls](https://img.shields.io/docker/pulls/hadolint/hadolint.svg)]() [![Docker Automated Build](https://img.shields.io/docker/automated/hadolint/hadolint.svg)]() [![Docker Build Status](https://img.shields.io/docker/build/hadolint/hadolint.svg)]()

# Hadolint Docker

This is Docker image for the [hadolint](https://github.com/hadolint/hadolint).

Default images include only `hadolint` static binary. All supported tags have also Debian based image alternative with `-debian` suffinx in tags.

## Supported tags

`scratch` based tiny images:
- `hadolint/hadolint:latest` tracks master branch
- `hadolint/hadolint:VERSION` refers release version, eg. `v1.9.0`
- `hadolint/hadolint:EXTENDED_VERSION` refers to the same version as `hadolint --version` with short git sha, eg. `v1.9.0-0-g4c4881a`

`debian` based images:
- `hadolint/hadolint:latest-debian` tracks master branch
- `hadolint/hadolint:VERSION-debian` refers release version, eg. `v1.9.0-debian`
- `hadolint/hadolint:EXTENDED_VERSION-debian` refers to the same version as `hadolint --version` with short git sha, eg. `v1.9.0-0-g4c4881a-debian`

Check out [Docker Hub](https://hub.docker.com/r/hadolint/hadolint/tags/) for available tags.

## Usage

To use this image, pull from Docker Hub, run the following command:

```bash
docker pull hadolint/hadolint
```

Verify the install

```bash
docker run --rm hadolint/hadolint hadolint --version
Haskell Dockerfile Linter v1.9.0-0-g4c4881a
```

or use a particular version number:

```bash
docker run --rm hadolint/hadolint:v1.9.0 hadolint --version
Haskell Dockerfile Linter v1.9.0-0-g4c4881a
```

Lint your `Dockerfile`:

```bash
docker run --rm -i hadolint/hadolint < Dockerfile
```

To exclude specific rules:

```bash
docker run --rm -i hadolint/hadolint hadolint \
  --ignore DL3003 \
  --ignore DL3006 \
  - < Dockerfile
```
