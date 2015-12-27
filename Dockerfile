FROM haskell:7.10
MAINTAINER Lukas Martinelli
RUN cabal update

RUN apt-get update \
 && apt-get install -y git \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/hadolint/
ADD ./hadolint.cabal /opt/hadolint/hadolint.cabal
RUN cabal install --only-dependencies -j4

ADD ./ /opt/hadolint
RUN cabal install

ENV PATH=/root/.cabal/bin:$PATH
CMD ["hadolint"]
