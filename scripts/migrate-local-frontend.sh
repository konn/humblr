#!/bin/bash
set -eux
cd _build/worker
npx wrangler r2 object put -f ../../workspace/test.html gohumblr-dev/test.html  --local
yes y | npx wrangler  d1 migrations apply gohumblr --local
npx wrangler  d1 execute gohumblr --file ../../humblr-worker/data/dummy.sql  --local

cat <<EOF >./.dev.vars
BASE_URL=http://localhost:8787
EOF