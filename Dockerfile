FROM haskell:8.0.2 as build
MAINTAINER Lukas Martinelli <me@lukasmartinelli.ch>

RUN apt-get update \
 && apt-get install --no-install-recommends -y \
    make=4.0-8.1 \
    xz-utils=5.1.1alpha+20120614-2+b3 \
    hlint=1.8.61-1+b2 \
 && rm -rf /var/lib/apt/lists/* \
 && cp /usr/lib/gcc/x86_64-linux-gnu/4.9/crtbeginS.o \
       /usr/lib/gcc/x86_64-linux-gnu/4.9/crtbeginT.o

WORKDIR /opt/hadolint/
COPY . /opt/hadolint
RUN stack setup \
 && stack install --local-bin-path /bin --ghc-options='-optl-static -optl-pthread' --force-dirty

ADD https://github.com/lalyos/docker-upx/releases/download/v3.91/upx /bin/upx
RUN chmod 755 /bin/upx \
 && upx --best --ultra-brute /bin/hadolint

FROM busybox:1.27.1
COPY --from=build /bin/hadolint /bin/hadolint

CMD ["hadolint", "-"]
