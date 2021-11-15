FROM debian:bullseye-slim AS builder

ENV LANG=C.UTF-8

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    build-essential=12.9 \
    libffi-dev=3.3* \
    libgmp-dev=2:6.2.* \
    libnuma1=2.* \
    libtinfo-dev=6.2* \
    libtinfo5=6.2* \
    libtinfo6=6.2* \
    llvm-9=1:9.0.1* \
    zlib1g-dev=1:1.2.* \
    curl=7.74.* \
    ca-certificates=* \
    git=1:2.30.* \
    netbase=6.3 \
    haskell-stack=2.3.3* \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/hadolint/
COPY stack.yaml package.yaml /opt/hadolint/
RUN stack --no-terminal --install-ghc test --only-dependencies

COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack install --flag hadolint:static

FROM debian:stretch-slim AS debian-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM alpine:3 AS alpine-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM scratch
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
