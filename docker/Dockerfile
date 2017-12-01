FROM debian:stretch-slim AS builder

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    build-essential=12.3 \
    libffi-dev=3.2.1-6 \
    libgmp-dev=2:6.1.2+dfsg-1 \
    zlib1g-dev=1:1.2.8.dfsg-5 \
    curl=7.52.1-5+deb9u3 \
    ca-certificates=20161130+nmu1 \
    git=1:2.11.0-3+deb9u2 \
    netbase=5.4 \
 && rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /opt/hadolint/
COPY hadolint.cabal stack.yaml /opt/hadolint/ 
RUN stack --no-terminal --install-ghc test --only-dependencies

COPY . /opt/hadolint
RUN scripts/fetch_version.sh \
  && stack install --ghc-options="-fPIC"

# COMPRESS WITH UPX
RUN curl -sSL https://github.com/upx/upx/releases/download/v3.94/upx-3.94-amd64_linux.tar.xz \
  | tar -x --xz --strip-components 1 upx-3.94-amd64_linux/upx \
  && ./upx --best --ultra-brute /root/.local/bin/hadolint

FROM busybox:1.27.2-glibc AS distro
COPY --from=builder /root/.local/bin/hadolint /bin/
CMD ["/bin/hadolint", "-"]
