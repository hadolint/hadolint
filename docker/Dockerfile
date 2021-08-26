FROM ubuntu:20.04 AS builder

SHELL ["/bin/bash", "-Eeuxo", "pipefail", "-c"]

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    build-essential=* \
    ca-certificates=* \
    curl=7.* \
    g++=* \
    gcc=* \
    git=1:2.* \
    gnupg=* \
    libc6-dev=* \
    libffi-dev=* \
    libffi7=* \
    libgmp-dev=* \
    libgmp-dev=* \
    libgmp10=* \
    libncurses-dev=* \
    libncurses5=* \
    libnuma-dev=* \
    libtinfo5=* \
    make=* \
    netbase=* \
    xz-utils=* \
    zlib1g-dev=* \
  && rm -fr /var/lib/apt/lists/*

WORKDIR /tmp

ENV \
  GHC_VERSION=8.10.4 \
  LLVM_VERSION=9.0.1 \
  STACK_VERSION=2.7.1

RUN \
  if [ "$(uname -m)" = aarch64 ]; then \
    curl -fsSL https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VERSION}/clang+llvm-${LLVM_VERSION}-aarch64-linux-gnu.tar.xz \
      | tar -xJvf - --strip-components=1 -C /usr \
    && curl -fL https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-aarch64.bin \
      -o /usr/local/bin/stack \
    && chmod +x /usr/local/bin/stack; \
  else \
    apt-get update \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && rm -fr /var/lib/apt/lists/*; \
  fi \
  && stack setup ghc-$GHC_VERSION

WORKDIR /opt/hadolint/
COPY stack.yaml package.yaml /opt/hadolint/
RUN stack -j"$(nproc)" --no-terminal test --only-dependencies

COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack -j"$(nproc)" --no-terminal install --ghc-options="-fPIC" \
    --flag hadolint:static

FROM debian:stretch-slim AS debian-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM alpine:3 AS alpine-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM scratch
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
