#!/bin/bash
set -eux

npx wrangler dev --port 9291 --local _build/workers/database/src/worker.js &

sleep 1
npx wrangler dev --port 9292 --local _build/workers/storage/src/worker.js &

sleep 1
npx wrangler dev --port 9293 --local _build/workers/ssr/src/worker.js &

sleep 1
npx wrangler dev --port 9294 --local _build/workers/images/src/worker.js   &

sleep 1
npx wrangler dev --local _build/workers/router/src/worker.js   &

sleep 2

[[ -f workspace/migration-dev.yaml ]] && [[ -d workspace/gohamblr-old ]] && cabal run -- tumblr-to-sqlite +RTS -N10 -RTS -c workspace/migration-dev.yaml workspace/gohamblr-old || echo "Some upload failed"

wait
