FROM lierdakil/alpine-haskell:9.4.8

WORKDIR /usr/src/kmonad/
RUN apk --no-cache add git
RUN stack update

COPY ./ ./
RUN sed -i '/ghc-options/ a\  $everything: -fPIC -split-sections' stack.yaml
RUN sed -i '/executable kmonad/ a\  ld-options: -static' kmonad.cabal
RUN stack --no-install-ghc --system-ghc --skip-ghc-check -j8 install --ghc-options=-j
