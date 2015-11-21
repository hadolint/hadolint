FROM haskell:7.10
MAINTAINER Lukas Martinelli
RUN cabal update

WORKDIR /opt/hadolint/
ADD ./hadolint.cabal /opt/hadolint/hadolint.cabal
RUN cabal install --only-dependencies -j4

ADD ./ /opt/hadolint
RUN cabal install

ENV PATH=/root/.cabal/bin:$PATH
CMD ["hadolint"]
