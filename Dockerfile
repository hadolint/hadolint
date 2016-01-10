FROM haskell:7.10
MAINTAINER Lukas Martinelli
RUN cabal update

RUN apt-get update \
 && apt-get install -y git \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/hadolint/
ADD ./hadolint.cabal /opt/hadolint/hadolint.cabal
ADD ./deps /opt/hadolint/deps
RUN cabal install hlint \
 && cabal install deps/shellcheck \
 && cabal install --only-dependencies -j4 --enable-tests \
 && cabal configure --enable-tests

ADD ./ /opt/hadolint
RUN cabal install

ENV PATH=/root/.cabal/bin:$PATH
EXPOSE 8000
CMD ["cabal", "run", "hadolint-api"]
