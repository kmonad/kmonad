FROM haskell:9-buster

WORKDIR /usr/src/kmonad/

COPY ./stack.yaml /usr/src/kmonad/
COPY ./kmonad.cabal /usr/src/kmonad/
RUN stack setup

COPY ./ /usr/src/kmonad/
RUN stack build

RUN \
  stack install && \
  chmod --verbose +x /root/.local/bin/kmonad
