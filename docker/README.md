[![Docker Pulls](https://img.shields.io/docker/pulls/hadolint/hadolint.svg)]() [![Docker Automated buil](https://img.shields.io/docker/automated/hadolint/hadolint.svg)]() [![Docker Build Status](https://img.shields.io/docker/build/hadolint/hadolint.svg)]()

# Hadolint Docker

This is Docker image for the [hadolint](https://github.com/hadolint/hadolint).

Images are based on [BusyBox with glibc](https://hub.docker.com/_/busybox/)  or [Debian](https://hub.docker.com/_/debian/) and include only `hadolint` static binary.

## Supported tags

- `hadolint/hadolint:latest` tracks master branch
- `hadolint/hadolint:VERSION` refers release version, eg. `v1.2.3`
- `hadolint/hadolint:EXTENDED_VERSION` refers to the same version as `hadolint --version` with short git sha, eg. `v1.2.3-0-g7df5f1c`

For each image, there is a Debian alternative with `-debian` suffix in a tag which can be useful in some corner cases, eg. UTF-8 characters in `Dockerfile`.

Check out [Docker Hub](https://hub.docker.com/r/hadolint/hadolint/tags/) for available tags.

## Usage

To use this image, pull from Docker Hub, run the following command:

```bash
docker pull hadolint/hadolint
```

Verify the install

```bash
docker run --rm hadolint/hadolint hadolint --version
Haskell Dockerfile Linter v1.2.3-2-gaf24cc3
```

or use a particular version number:

```bash
docker run --rm hadolint/hadolint:v1.2.3 hadolint --version
Haskell Dockerfile Linter v1.2.3-0-g7df5f1c
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
