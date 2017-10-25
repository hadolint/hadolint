FROM debian:stretch-slim as builder

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    build-essential=12.3 \
    libffi-dev=3.2.1-6 \
    libgmp-dev=2:6.1.2+dfsg-1 \
    zlib1g-dev=1:1.2.8.dfsg-5 \
    curl=7.52.1-5+deb9u2 \
    ca-certificates=20161130+nmu1 \
    git=1:2.11.0-3+deb9u2 \
    netbase=5.4 \
 && rm -rf /var/lib/apt/lists/*

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /opt/hadolint/
COPY . /opt/hadolint
RUN cp /usr/lib/gcc/x86_64-linux-gnu/6/crtbeginS.o /usr/lib/gcc/x86_64-linux-gnu/6/crtbeginT.o
RUN stack install --install-ghc --ghc-options '-optl-static -fPIC -optc-Os -optl-pthread'

ENV PATH="/opt/hadolint/.stack-work/install/x86_64-linux/lts-4.1/7.10.3/bin:$PATH"
CMD ["hadolint", "-"]
