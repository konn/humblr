#!/bin/bash
set -eux

npx wrangler dev --port 9291 --local _build/database/src/worker.js &

sleep 1
npx wrangler dev --port 9292 --local _build/storage/src/worker.js &

sleep 1
npx wrangler dev --port 9293 --local _build/ssr/src/worker.js &

sleep 1
npx wrangler dev --local _build/router/src/worker.js   &

wait
