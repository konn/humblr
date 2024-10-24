{-# LANGUAGE OverloadedStrings #-}

module Language.Javascript.JSaddle.Runner (run) where

import Data.ByteString qualified as BS
import Data.Default (def)
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WKWebView qualified as Raw

run :: JSM () -> IO ()
run act = do
  src <- BS.readFile "humblr-frontend/data/index.html"
  Raw.runHTMLWithBaseURL src "https://humblr.example.com" def act
