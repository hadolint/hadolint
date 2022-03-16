FROM debian:sid-slim AS base

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    libffi-dev=* \
    libgmp-dev=* \
    zlib1g-dev=* \
    curl=* \
    ca-certificates=* \
    git=* \
    netbase=* \
    curl=7.* \
    libtinfo6=6.* \
    libtinfo-dev=6.* \
    libnuma1=2.* \
    libnuma-dev=2.* \
    clang-12=* \
    lld-12=* \
    llvm-12=* \
    make=* \
 && rm -rf /var/lib/apt/lists/*

RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-12 100 \
 && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-12 100 \
 && update-alternatives --install /usr/bin/ld ld /usr/bin/ld.lld-12 100 \
 && update-alternatives --install /usr/bin/llc llc /usr/bin/llc-12 100 \
 && update-alternatives --install /usr/bin/opt opt /usr/bin/opt-12 100 \
 && update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-12 100

ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    LDFLAGS="-L/usr/lib/llvm-12/lib" \
    CPPFLAGS="-I/usr/lib/llvm-12/include"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/root/.ghcup/bin:${PATH}" \
    LC_ALL=C.UTF-8


FROM base AS builder
WORKDIR /opt/hadolint/
COPY stack.yaml package.yaml /opt/hadolint/
RUN stack build --no-terminal --no-install-ghc --system-ghc --only-dependencies --ghc-options=-fllvm


COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack build --no-install-ghc --system-ghc --ghc-options=-fllvm \
  && stack install --no-install-ghc --system-ghc --ghc-options=-fllvm


FROM debian:sid-slim AS compressor
RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    upx-ucl=* \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/hadolint /bin/
RUN upx /bin/hadolint


FROM debian:sid-slim AS result-debian

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    libnuma1=2.* \
 && rm -rf /var/lib/apt/lists/*
COPY --from=compressor /bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
