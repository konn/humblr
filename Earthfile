VERSION 0.8
ARG --global GHC_VER=9.10.0.20240412
FROM --platform=linux/amd64 ghcr.io/konn/humblr/build:${GHC_VER}
WORKDIR /workdir

ENV GHC=wasm32-wasi-ghc
ENV CABAL=wasm32-wasi-cabal --project-file=cabal-wasm.project --with-ghc=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs

base-image:
  FROM --platform=linux/amd64 ghcr.io/konn/ghc-wasm-earthly:${GHC_VER}
  RUN wasm32-wasi-cabal --with-ghc=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs update --index-state=2024-07-23T12:15:54Z
  SAVE IMAGE --push ghcr.io/konn/humblr/build:${GHC_VER}

build:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  ENV MOUNT_GLOBAL_STORE="type=cache,mode=0777,id=${target}#ghc-${GHC_VER}#global-store,sharing=shared,target=/root/.ghc-wasm/.cabal/store"
  ENV MOUNT_DIST_NEWSTYLE="type=cache,mode=0777,id=${target}#ghc${GHC_VER}#dist-newstyle,sharing=shared,target=dist-newstyle"
  COPY --keep-ts . .
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} update --index-state=2024-06-28T07:25:12Z
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build --only-dependencies ${target}
  RUN --mount ${MOUNT_GLOBAL_STORE} \
      --mount ${MOUNT_DIST_NEWSTYLE} \
      ${CABAL} build  ${target}
  # From frontend/build.sh in tweag/ghc-wasm-miso-examples
  LET HS_WASM_PATH=$(${CABAL} list-bin -v0 ${target})
  LET WASM_LIB=$(wasm32-wasi-ghc --print-libdir)
  LET DEST=dist/${wasm}
  RUN mkdir -p dist
  RUN --mount ${MOUNT_DIST_NEWSTYLE} cp ${HS_WASM_PATH} ./dist/${wasm}
  RUN --mount ${MOUNT_DIST_NEWSTYLE} ${WASM_LIB}/post-link.mjs --input ${HS_WASM_PATH} --output ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT dist

optimised-wasm:
  ARG target
  ARG outdir=$(echo ${target} | cut -d: -f3)
  ARG wasm=${outdir}.wasm
  RUN mkdir -p dist/
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
  COPY  (+optimised-wasm/dist --target=${target} --outdir=${outdir} --wasm=${wasm}) ./dist
  LET PATCHER=./js-ffi-patcher.mjs
  COPY ./cloudflare-kv-manager/data/jsffi-patcher.mjs ${PATCHER}
  RUN node ${PATCHER} ./dist/ghc_wasm_jsffi.js
  SAVE ARTIFACT ./dist

main:
  COPY cloudflare-kv-manager/data/worker-template/ ./dist/
  COPY (+patch-jsffi-for-cf/dist --target=cloudflare-kv-manager:exe:cloudflare-kv-manager --wasm=handlers.wasm) ./dist/src
  RUN cd ./dist && npm i
  SAVE ARTIFACT ./dist AS LOCAL _build/
