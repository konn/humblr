VERSION 0.8
ARG --global GHC_VER=9.10.1
ARG --global GLOBAL_CACHE_IMAGE=ghcr.io/konn/shortener/build-cache
FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project \
--with-compiler=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc=wasm32-wasi-ghc-${GHC_VER} \
--with-ghc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hc-pkg=wasm32-wasi-ghc-pkg-${GHC_VER} \
--with-hsc2hs=wasm32-wasi-hsc2hs-${GHC_VER}

build-all:
  COPY --keep-ts ./*.project ./
  COPY --keep-ts ./*.freeze ./
  COPY --keep-ts ./humblr-core ./humblr-core
  COPY --keep-ts ./humblr-frontend ./humblr-frontend
  COPY --keep-ts ./humblr-workers ./humblr-workers
  CACHE --sharing shared --chmod 0777 --id=all#ghc-${GHC_VER}#global-store --persist /root/.ghc-wasm/.cabal/store
  CACHE --sharing=shared --chmod=0777 --id=all#ghc${GHC_VER}#dist-newstyle --persist dist-newstyle
  RUN ${CABAL} update --index-state=2024-10-17T07:25:36Z
  RUN ${CABAL} build --only-dependencies all
  RUN ${CABAL} build all

build:
  FROM +build-all
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin -v0 ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist/${wasm}
  RUN mkdir -p dist
  RUN cp ${HS_WASM_PATH} ./dist/${wasm}
  RUN ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT dist

optimised-wasm:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  RUN mkdir -p dist/
  BUILD --platform=linux/amd64 +build --target=${target} --outdir=${outdir} --wasm=${wasm}.orig
  COPY (+build/dist/${wasm}.orig --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/${wasm} dist/${wasm}.orig
  RUN wasm-opt -Oz dist/${wasm} -o dist/${wasm}
  RUN wasm-tools strip -o dist/${wasm} dist/${wasm}
  COPY (+build/dist/ghc_wasm_jsffi.js --target=${target} --outdir=${outdir} --wasm=${wasm}.orig) ./dist/
  RUN rm ./dist/${wasm}.orig
  SAVE ARTIFACT dist

patch-jsffi-for-cf:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  BUILD --platform=linux/amd64 +optimised-wasm --target=${target} --outdir=${outdir} --wasm=${wasm}
  COPY  (+optimised-wasm/dist --target=${target} --outdir=${outdir} --wasm=${wasm}) ./dist
  LET PATCHER=./js-ffi-patcher.mjs
  COPY ./build-scripts/jsffi-patcher.mjs ${PATCHER}
  RUN node ${PATCHER} ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT ./dist

frontend:
  BUILD --platform=linux/amd64 +optimised-wasm --target=humblr-frontend:exe:humblr-frontend
  COPY (+optimised-wasm/dist --target=humblr-frontend:exe:humblr-frontend) ./dist
  LET ORIG_WASM=humblr-frontend.wasm
  LET SHASUM_WASM=$(sha1sum dist/${ORIG_WASM} | cut -c1-7)
  LET FINAL_WASM=humblr-frontend-${SHASUM_WASM}.wasm
  RUN mv dist/${ORIG_WASM} dist/${FINAL_WASM}

  LET GHC_JSFFI_ORIG=ghc_wasm_jsffi.js
  LET SHASUM_JSFFI=$(sha1sum dist/${GHC_JSFFI_ORIG} | cut -c1-7)
  LET GHC_JSFFI_FINAL=ghc_wasm_jsffi-${SHASUM_JSFFI}.js
  RUN mv dist/${GHC_JSFFI_ORIG} dist/${GHC_JSFFI_FINAL}

  COPY humblr-frontend/data/index.js dist/index.js
  RUN sed -i "s/${ORIG_WASM}/${FINAL_WASM}/g" dist/index.js
  RUN sed -i "s/${GHC_JSFFI_ORIG}/${GHC_JSFFI_FINAL}/g" dist/index.js
  LET INDEX_JS_SHASUM=$(sha1sum dist/index.js | cut -c1-7)
  LET INDEX_JS_FINAL=index-${INDEX_JS_SHASUM}.js
  RUN mv dist/index.js dist/${INDEX_JS_FINAL}
  COPY humblr-frontend/data/index.html dist/index.html
  RUN sed -i "s/index.js/${INDEX_JS_FINAL}/g" dist/index.html
  RUN echo -n "${INDEX_JS_FINAL}" | jq -R -M -c '{script: .}' > dist/assets.json
  SAVE ARTIFACT ./dist

build-worker:
  ARG target
  COPY humblr-workers/data/worker-template/ ./dist/
  COPY humblr-workers/data/wrangler-configs/${target}/* ./dist/
  BUILD --platform=linux/amd64  +patch-jsffi-for-cf --target=humblr-workers:exe:${target} --wasm=worker.wasm
  COPY (+patch-jsffi-for-cf/dist --target=humblr-workers:exe:${target} --wasm=worker.wasm) ./dist/src
  RUN cd ./dist && npm i
  SAVE ARTIFACT ./dist

all:
  BUILD  --platform=linux/amd64 +build-all
  # Build database worker
  BUILD --platform=linux/amd64 +build-worker --target=humblr-database --wasm=worker.wasm
  COPY (+build-worker/dist --target=humblr-database) ./dist/database

  # Build storage worker
  BUILD --platform=linux/amd64 +build-worker --target=humblr-storage --wasm=worker.wasm
  COPY (+build-worker/dist --target=humblr-storage) ./dist/storage

  # Build SSR worker
  BUILD --platform=linux/amd64 +build-worker --target=humblr-storage --wasm=worker.wasm
  COPY (+build-worker/dist --target=humblr-ssr) ./dist/ssr

  # Build Images worker
  BUILD --platform=linux/amd64 +build-worker --target=humblr-storage --wasm=worker.wasm
  COPY (+build-worker/dist --target=humblr-images) ./dist/images

  # Build Router worker
  BUILD --platform=linux/amd64 +build-worker --target=humblr-router --wasm=worker.wasm
  COPY (+build-worker/dist --target=humblr-router) ./dist/router

  # Place frontend in the router assets
  BUILD  --platform=linux/amd64 +frontend
  COPY +frontend/dist/* ./dist/router/assets/assets/
  COPY +frontend/dist/assets.json ./dist/ssr/assets/
  SAVE ARTIFACT ./dist AS LOCAL _build
  SAVE IMAGE --push "${GLOBAL_CACHE_IMAGE}:cache"
