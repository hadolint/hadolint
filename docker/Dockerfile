FROM debian:stretch-slim AS stack-build-env-amd64

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    build-essential=12.3 \
    libffi-dev=3.2.* \
    libgmp-dev=2:6.1.* \
    zlib1g-dev=1:1.2.* \
    curl=7.52.* \
    ca-certificates=* \
    git=1:2.11.* \
    netbase=5.4 \
 && curl -sSL https://get.haskellstack.org/ | sh \
 && rm -rf /var/lib/apt/lists/*

# COPY from https://github.com/commercialhaskell/stack/blob/master/etc/dockerfiles/arm64.Dockerfile#L3-L5
FROM ubuntu:20.04 AS stack-build-env-arm64

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl=7.* \
    ca-certificates=* \
    build-essential=* \
    libffi-dev=* \
    git=1:2.* \
    netbase=* \
    libffi7=* \
    libgmp-dev=* \
    libgmp10=* \
    libncurses-dev=* \
    libncurses5=* \
    libtinfo5=* \
    libnuma-dev=* \
    xz-utils=* \
    g++=* \
    gcc=* \
    libc6-dev=* \
    libgmp-dev=* \
    make=* \
    zlib1g-dev=* \
    gnupg=*


WORKDIR /tmp

ENV LLVM_VERSION=9.0.1

RUN curl -L https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/clang+llvm-${LLVM_VERSION}-aarch64-linux-gnu.tar.xz --output /tmp/llvm.tar.xz \
 && unxz /tmp/llvm.tar.xz \
 && tar xfv /tmp/llvm.tar --strip-components 1 -C /usr \
 && rm /tmp/llvm.tar \
 && curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.7.1/stack-2.7.1-linux-"$(uname -m)".bin > /usr/local/bin/stack \
 && chmod +x /usr/local/bin/stack

# hadolint ignore=DL3006
FROM stack-build-env-${TARGETARCH} AS builder

WORKDIR /opt/hadolint/
COPY stack.yaml package.yaml /opt/hadolint/
RUN stack --no-terminal --install-ghc test --only-dependencies

COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack install --ghc-options="-fPIC" --flag hadolint:static

FROM debian:stretch-slim AS debian-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM alpine:3 AS alpine-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM scratch
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
