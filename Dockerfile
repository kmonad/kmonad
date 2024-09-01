FROM lierdakil/alpine-haskell:9.4.8

WORKDIR /usr/src/kmonad/
RUN apk --no-cache add git
RUN stack update

COPY ./kmonad.cabal ./stack.yaml ./
RUN stack --no-install-ghc --system-ghc --skip-ghc-check -j8 build --only-dependencies --ghc-options="-fPIC"
COPY ./ ./
RUN sed -i '/executable kmonad/ a\  ld-options: -static' kmonad.cabal && \
  stack --no-install-ghc --system-ghc --skip-ghc-check install --ghc-options="-j -fPIC"
