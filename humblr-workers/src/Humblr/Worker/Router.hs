{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module Humblr.Worker.Router (handlers, JSObject (..), JSHandlers) where

import Control.Exception.Safe (throwString)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Wasm.Object.Builtins
import Humblr.Types
import Humblr.Worker.Database (DatabaseServiceClass)
import Humblr.Worker.Storage (StorageServiceClass)
import Network.Cloudflare.Worker.Binding hiding (getBinding, getSecret)
import Network.Cloudflare.Worker.Binding qualified as Raw
import Network.Cloudflare.Worker.Binding.Assets (AssetsClass)
import Network.Cloudflare.Worker.Binding.Assets qualified as RawAssets
import Network.Cloudflare.Worker.Request qualified as Req
import Network.URI
import Servant.Auth.Cloudflare.Workers
import Servant.Cloudflare.Workers.Assets (serveAssets)
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCachedRaw)
import Servant.Cloudflare.Workers.Cache qualified as Cache
import Servant.Cloudflare.Workers.Generic (AsWorker, genericCompileWorkerContext)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude hiding (inject)

type App = Handler HumblrEnv

type HumblrEnv =
  BindingsClass
    '["BASE_URL", "CF_TEAM_NAME"]
    '["CF_AUD_TAG"]
    '[ '("Storage", StorageServiceClass)
     , '("Database", DatabaseServiceClass)
     , '("ASSETS", AssetsClass)
     ]

assetCacheOptions :: CacheOptions
assetCacheOptions =
  CacheOptions
    { cacheTTL = 3600 * 24
    , onlyOk = True
    , includeQuery = False
    }

handlers :: IO JSHandlers
handlers =
  genericCompileWorkerContext @HumblrEnv
    ( \env _ -> do
        let audience = do
              let aud = Raw.getSecret "CF_AUD_TAG" env
              guard $ not $ T.null aud
              pure aud
        team0 <- case A.fromJSON $ Raw.getEnv "CF_TEAM_NAME" env of
          J.Error e -> throwString $ "Could not parse CF_TEAM_NAME: " <> e
          J.Success x -> pure x
        let team = do
              guard $ not $ null team0
              pure team0
        !sett <- defaultCloudflareZeroTrustSettings audience team
        let !jwt = toJWTSettings sett
        pure $ sett :. jwt :. EmptyContext
    )
    workers

workers :: RootAPI (AsWorker HumblrEnv)
workers =
  RootAPI
    { frontend = frontend
    , assets = serveCachedRaw assetCacheOptions $ serveAssets "ASSETS"
    , apiRoutes = apiRoutes
    , resources = resources
    }

resources :: Worker HumblrEnv Raw
resources = Cache.serveCachedRaw assetCacheOptions $ Tagged \req env _ -> do
  let storage = Raw.getBinding "Storage" env
      pth = T.intercalate "/" req.pathInfo
  consoleLog $ fromText $ "Fetching resource: " <> pth
  resp <- await' =<< storage.get pth
  maybe (toWorkerResponse $ responseServerError err404 {errBody = "Not Found: " <> TE.encodeUtf8 (Req.getUrl req.rawRequest)}) pure resp

frontend :: FrontendRoutes (AsWorker HumblrEnv)
frontend =
  FrontendRoutes
    { topPage = const serveIndex
    , tagArticles = const $ const serveIndex
    , newArticle = serveIndex
    , editArticle = const serveIndex
    , articlePage = const serveIndex
    , adminHome = const serveIndex
    }

serveIndex :: Worker HumblrEnv Raw
serveIndex = Tagged \req env _ ->
  if not $ null req.pathInfo
    then toWorkerResponse $ responseServerError err404 {errBody = "Not Found"}
    else do
      let link = "/" <> toUrlPiece rootApiLinks.assets <> "/index.html"
          rawUrl = Req.getUrl req.rawRequest
          !url = fromString @USVString $ show $ (fromMaybe (error $ "Invalid Url: " <> show rawUrl) $ parseURI $ T.unpack rawUrl) {uriPath = T.unpack link}
      resp <- await =<< RawAssets.fetch (Raw.getBinding "ASSETS" env) (inject url)
      pure resp

protectIfConfigured ::
  AuthResult User ->
  App a ->
  App a
protectIfConfigured auth act = do
  audience <- getSecret "CF_AUD_TAG"
  if T.null audience
    then act
    else case auth of
      Authenticated User {} -> act
      _ -> serverError err403 {errBody = "Unauthorised"}

apiRoutes ::
  RestApi (AsWorker HumblrEnv)
apiRoutes =
  RestApi
    { listTagArticles = listTagArticles
    , listArticles = listArticles
    , getArticle = getArticle
    , adminAPI
    }

adminAPI :: AuthResult User -> AdminAPI (AsWorker HumblrEnv)
adminAPI usr =
  AdminAPI
    { putArticle = putArticle usr
    , postArticle = postArticle usr
    , deleteArticle = deleteArticle usr
    }

putArticle :: AuthResult User -> T.Text -> ArticleUpdate -> Handler HumblrEnv NoContent
putArticle user slug upd = protectIfConfigured user do
  db <- getBinding "Database"
  liftIO $ await' =<< db.putArticle slug upd
  pure NoContent

deleteArticle :: AuthResult User -> T.Text -> Handler HumblrEnv NoContent
deleteArticle user slug = protectIfConfigured user do
  db <- getBinding "Database"
  liftIO $ await' =<< db.deleteArticle slug
  pure NoContent

postArticle :: AuthResult User -> ArticleSeed -> App NoContent
postArticle user art = protectIfConfigured user do
  db <- getBinding "Database"
  liftIO $ await' =<< db.postArticle art
  pure NoContent

getArticle :: T.Text -> App Article
getArticle slug = do
  db <- getBinding "Database"
  p <- liftIO $ db.getArticle slug
  liftIO $ await' p

listTagArticles :: T.Text -> Maybe Word -> App [Article]
listTagArticles tag mpage = do
  db <- getBinding "Database"
  liftIO $ await' =<< db.listTagArticles tag mpage

listArticles :: Maybe Word -> App [Article]
listArticles mpage = do
  db <- getBinding "Database"
  liftIO $ await' =<< db.listArticles mpage

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()
