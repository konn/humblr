#!/bin/bash
set -eux
cd _build/database
npx wrangler r2 object put -f ../../workspace/test.html gohumblr-dev/test.html  --local
yes y | npx wrangler  d1 migrations apply gohumblr --local
npx wrangler  d1 execute gohumblr --file ../../humblr-workers/data/dummy.sql  --local
