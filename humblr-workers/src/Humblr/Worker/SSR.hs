{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Worker.SSR (SSRServiceClass, JSObject (..), handlers, SSRService) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Response qualified as Raw
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Humblr.CMark (getSummary)
import Humblr.CMark qualified as CM
import Humblr.Frontend.Types
import Humblr.Frontend.View (viewModel)
import Humblr.Worker.Database (DatabaseServiceClass)
import Lucid
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.FetchAPI (fetch)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Streaming.ByteString qualified as Q

data SSRServiceFuns = SSRServiceFuns {renderArticle :: T.Text -> App WorkerResponse}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '["ROOT_URI"] '[] '[ '("Database", DatabaseServiceClass)]

type SSRFuns = Signature SSREnv SSRServiceFuns

type SSRServiceClass = ServiceClass SSRFuns

type SSRService = JSObject SSRServiceClass

handlers :: IO SSRService
handlers = toService @SSREnv SSRServiceFuns {renderArticle}

newtype Assets = Assets {script :: T.Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (A.FromJSON)

renderArticle :: T.Text -> App WorkerResponse
renderArticle slug = do
  db <- getBinding "Database"
  root <- getEnv "ROOT_URI"
  liftIO do
    art <- await' =<< db.getArticle slug
    resp <-
      Q.toStrict_ $
        Q.mwrap $
          fmap (nullable mempty fromReadableStream) $
            Raw.js_get_body
              =<< await
              =<< fetch (inject @USVStringClass $ fromText $ root <> "/assets/assets.json") none
    let script = "/assets/" <> maybe "index.js" (.script) (A.decodeStrict @Assets resp)
        body = renderBS do
          doctype_
          html_ [data_ "theme" "dark"] do
            head_ do
              meta_ [charset_ "utf-8"]
              meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
              meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
              title_ do
                toHtml $
                  CM.nodeToPlainText
                    (fromMaybe <$> id <*> getSummary $ CM.commonmarkToNode [] $ art.body)
                " - ごはんぶらー"
              link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css"]
              link_ [rel_ "stylesheet", type_ "text/css", href_ "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,0,0"]
              script_ mempty `with` [src_ script, type_ "module", async_ ""]

            body_ $
              toHtml $
                viewModel
                  Model
                    { mode = ArticlePage art
                    , modal = Nothing
                    , errorMessage = Nothing
                    }
    Resp.newResponse
      Resp.SimpleResponseInit
        { status = 200
        , body = Just $ Resp.WorkerResponseLBS body
        , statusText = "Ok"
        , headers = M.fromList [("Last-Modified", BS8.pack $ formatTime defaultTimeLocale rfc822DateFormat $ art.updatedAt)]
        }
