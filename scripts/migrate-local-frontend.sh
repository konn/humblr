#!/bin/bash
set -eux

pushd _build/storage
npx wrangler r2 object put -f ../../workspace/test.html gohumblr-dev/test.html  --local
popd

pushd _build/database
yes y | npx wrangler  d1 migrations apply gohumblr --local
npx wrangler  d1 execute gohumblr --file ../../humblr-workers/data/dummy.sql  --local || echo "Already there"
