FROM debian:stretch-slim AS builder

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

WORKDIR /opt/hadolint/
COPY stack.yaml package.yaml /opt/hadolint/
RUN stack --no-terminal --install-ghc test --only-dependencies

COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack install --ghc-options="-fPIC" --flag hadolint:static

# COMPRESS WITH UPX
RUN curl -sSL https://github.com/upx/upx/releases/download/v3.94/upx-3.94-amd64_linux.tar.xz \
  | tar -x --xz --strip-components 1 upx-3.94-amd64_linux/upx \
  && ./upx --best --ultra-brute /root/.local/bin/hadolint

FROM debian:stretch-slim AS debian-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM scratch AS distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]

FROM alpine:3 AS alpine-distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
