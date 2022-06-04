FROM lierdakil/alpine-haskell:8.10.7

WORKDIR /usr/src/kmonad/
RUN apk --no-cache add git
RUN stack update

COPY ./kmonad.cabal ./
COPY ./static/stack.yaml ./static/
COPY ./stack.yaml ./
RUN cat ./static/stack.yaml >> stack.yaml && \
  stack --no-install-ghc --system-ghc --skip-ghc-check -j8 build --only-dependencies
COPY ./ ./
RUN cat ./static/stack.yaml >> stack.yaml && \
  stack --no-install-ghc --system-ghc --skip-ghc-check install --ghc-options -j
