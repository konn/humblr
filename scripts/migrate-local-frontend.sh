#!/bin/bash
set -eux

pushd _build/storage
npx wrangler r2 object put -f ../../workspace/test.html gohumblr-dev/test.html  --local
npx wrangler@latest kv  key put --local --binding "KV"  "URL_SIGN_KEY" '{"key_ops":["sign","verify"],"ext":false,"kty":"oct","k":"asmOuAGYKZ1iG0ufo0xbyY0iFM65oqH9WGiVp3Ln365BBIZZKt08z4wJel6AnxvtMYNueUny6vuEfZEwvqO-rA","alg":"HS256"}'
popd

pushd _build/storage-indep
npx wrangler r2 object put -f ../../workspace/test.html gohumblr-dev/test.html  --local
npx wrangler@latest kv  key put --local --binding "KV"  "URL_SIGN_KEY" '{"key_ops":["sign","verify"],"ext":false,"kty":"oct","k":"asmOuAGYKZ1iG0ufo0xbyY0iFM65oqH9WGiVp3Ln365BBIZZKt08z4wJel6AnxvtMYNueUny6vuEfZEwvqO-rA","alg":"HS256"}'
popd

pushd _build/database
yes y | npx wrangler  d1 migrations apply gohumblr --local
npx wrangler  d1 execute gohumblr --file ../../humblr-workers/data/dummy.sql  --local || echo "Already there"
