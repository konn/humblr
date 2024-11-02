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

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Humblr.CMark (getSummary)
import Humblr.CMark qualified as CM
import Humblr.Frontend.Types
import Humblr.Frontend.View (viewModel)
import Humblr.Worker.Database (DatabaseServiceClass)
import Lucid
import Lucid.Base qualified as L
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Assets (AssetsClass)
import Network.Cloudflare.Worker.Binding.Assets qualified as Assets
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Servant.Cloudflare.Workers.Prelude (Link, toUrlPiece)
import Streaming.ByteString qualified as Q

data SSRServiceFuns = SSRServiceFuns {renderArticle :: T.Text -> App WorkerResponse}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '["ROOT_URI"] '[] '[ '("Database", DatabaseServiceClass), '("ASSETS", AssetsClass)]

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
  assets <- getBinding "ASSETS"
  root <- getEnv "ROOT_URI"
  liftIO do
    art <- await' =<< db.getArticle slug
    resp <-
      Q.toStrict_ $
        Q.mwrap $
          fmap (nullable mempty fromReadableStream) $
            Resp.getBody
              =<< await
              =<< Assets.fetch assets (inject @USVStringClass $ fromText $ root <> "/assets.json")
    let script = "/assets/" <> maybe "index.js" (.script) (A.decodeStrict @Assets resp)
        summary =
          CM.nodeToPlainText
            (fromMaybe <$> id <*> getSummary $ CM.commonmarkToNode [] $ art.body)

        body = renderBS do
          doctype_
          html_ [data_ "theme" "dark"] do
            head_ do
              meta_ [charset_ "utf-8"]
              meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
              meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
              title_ $ toHtml summary <> " - ごはんぶらー"
              meta_ [name_ "twitter:card", content_ "summary"]
              meta_ [property_ "og:url", content_ $ linkWithRoot root $ rootApiLinks.frontend.articlePage slug]
              meta_ [property_ "og:title", content_ summary]
              meta_ [property_ "og:description", content_ summary]
              forM_ (listToMaybe art.attachments) \att ->
                meta_ [property_ "og:image", content_ $ linkWithRoot root $ rootApiLinks.images OGP $ T.splitOn "/" att.name]

              link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css"]
              link_ [rel_ "stylesheet", type_ "text/css", href_ "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,0,0"]
              script_ mempty `with` [src_ script, type_ "module", async_ "", defer_ ""]

            body_ $
              toHtml $
                viewModel Model {mode = ArticlePage art, modal = Nothing, errorMessage = Nothing}
    Resp.newResponse
      Resp.SimpleResponseInit
        { status = 200
        , body = Just $ Resp.WorkerResponseLBS body
        , statusText = "Ok"
        , headers = M.fromList [("Last-Modified", BS8.pack $ formatTime defaultTimeLocale rfc822DateFormat $ art.updatedAt)]
        }

linkWithRoot :: T.Text -> Link -> T.Text
linkWithRoot root l =
  T.dropWhileEnd (== '/') root
    <> "/"
    <> toUrlPiece l

property_ :: T.Text -> Attribute
property_ = L.Attribute "property"
