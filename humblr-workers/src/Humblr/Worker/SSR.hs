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
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import GHC.Wasm.Object.Builtins
import Humblr.Frontend.Types
import Humblr.Frontend.View (viewModel)
import Humblr.Worker.Database (DatabaseServiceClass)
import Lucid
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp

data SSRServiceFuns = SSRServiceFuns {renderArticle :: T.Text -> App WorkerResponse}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '[] '[] '[ '("Database", DatabaseServiceClass)]

type SSRFuns = Signature SSREnv SSRServiceFuns

type SSRServiceClass = ServiceClass SSRFuns

type SSRService = JSObject SSRServiceClass

handlers :: IO SSRService
handlers = toService @SSREnv SSRServiceFuns {renderArticle}

renderArticle :: T.Text -> App WorkerResponse
renderArticle slug = do
  db <- getBinding "Database"
  liftIO do
    art <- await' =<< db.getArticle slug
    let body =
          renderBS $
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
