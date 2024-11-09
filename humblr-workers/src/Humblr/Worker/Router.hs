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
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Worker.Router (handlers, JSObject (..), JSHandlers) where

import Control.Exception.Safe (throwString)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.Bifunctor qualified as Bi
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.ReadableStream (ReadableStream)
import Humblr.Types
import Humblr.Worker.Database (DatabaseServiceClass)
import Humblr.Worker.Images (ImagesServiceClass)
import Humblr.Worker.SSR (SSRServiceClass)
import Humblr.Worker.Storage (GetParams (..), StorageServiceClass)
import Network.Cloudflare.Worker.Binding hiding (getBinding, getSecret)
import Network.Cloudflare.Worker.Binding qualified as Raw
import Network.Cloudflare.Worker.Binding.Assets (AssetsClass)
import Network.Cloudflare.Worker.Binding.Assets qualified as RawAssets
import Network.Cloudflare.Worker.Request qualified as Req
import Servant.Auth.Cloudflare.Workers
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCachedRaw)
import Servant.Cloudflare.Workers.Cache qualified as Cache
import Servant.Cloudflare.Workers.Generic (AsWorker, genericCompileWorkerContext)
import Servant.Cloudflare.Workers.Internal.Delayed (addBodyCheck)
import Servant.Cloudflare.Workers.Internal.DelayedIO (delayedFail, withRequest)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude hiding (inject)

type App = Handler HumblrEnv

type HumblrEnv =
  BindingsClass
    '["ROOT_URI", "CF_TEAM_NAME"]
    '["CF_AUD_TAG"]
    '[ '("Storage", StorageServiceClass)
     , '("Database", DatabaseServiceClass)
     , '("SSR", SSRServiceClass)
     , '("ASSETS", AssetsClass)
     , '("IMAGES", ImagesServiceClass)
     ]

imageCacheOptions :: CacheOptions
imageCacheOptions =
  CacheOptions
    { cacheTTL = 14 * 24 * 3600
    , onlyOk = True
    , includeQuery = False
    }

articleCacheOptions :: CacheOptions
articleCacheOptions =
  CacheOptions
    { cacheTTL = 7 * 24 * 3600
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
    , assets = assetsFallback
    , apiRoutes = apiRoutes
    , images = imagesRoutes
    , resources = resources
    }

imagesRoutes :: ImagesAPI (AsWorker HumblrEnv)
imagesRoutes =
  ImagesAPI
    { thumb = serveCachedRaw imageCacheOptions . serveImageSized Thumb
    , large = serveCachedRaw imageCacheOptions . serveImageSized Large
    , ogp = serveCachedRaw imageCacheOptions . serveImageSized Ogp
    , medium = serveCachedRaw imageCacheOptions . serveImageSized Medium
    , twitter = serveCachedRaw imageCacheOptions . serveImageSized Twitter
    }

assetsFallback :: Worker HumblrEnv Raw
assetsFallback = Tagged \_ _ _ ->
  -- assets must be handled by assets, so if something reach here,
  -- it must be 404
  toWorkerResponse $ responseServerError err404

serveImageSized :: ImageSize -> [T.Text] -> Worker HumblrEnv Raw
serveImageSized sz paths = Tagged \_ env _ -> do
  let images = Raw.getBinding "IMAGES" env
  await' =<< images.get sz paths

resources :: [T.Text] -> POSIXTime -> T.Text -> Worker HumblrEnv Raw
resources paths expiry sign = Cache.serveCachedRaw imageCacheOptions $ Tagged \_ env _ -> do
  let storage = Raw.getBinding "Storage" env
  await' =<< storage.get GetParams {..}

frontend :: FrontendRoutes (AsWorker HumblrEnv)
frontend =
  FrontendRoutes
    { topPage = const serveIndex
    , tagArticles = const $ const serveIndex
    , newArticle = serveIndex
    , editArticle = const serveIndex
    , articlePage = articlePage
    , adminHome = const serveIndex
    }

articlePage :: T.Text -> Worker HumblrEnv Raw
articlePage slug = Cache.serveCachedRaw articleCacheOptions $ Tagged \_ env _ -> do
  let renderer = Raw.getBinding "SSR" env
  await . jsPromise =<< renderer.renderArticle slug

serveIndex :: Worker HumblrEnv Raw
serveIndex = Tagged \_ env _ -> do
  let root = fromSuccess $ J.fromJSON $ Raw.getEnv "ROOT_URI" env
      link = "/" <> toUrlPiece rootApiLinks.assets <> "/index.html"
      !url = fromText @USVStringClass $ root <> link
  resp <- await =<< RawAssets.fetch (Raw.getBinding "ASSETS" env) (inject url)
  pure resp

fromSuccess :: J.Result a -> a
{-# INLINE fromSuccess #-}
fromSuccess (J.Success a) = a
fromSuccess _ = error "Could not happen"

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

instance (HasWorker e api ctxs) => HasWorker e (Image :> api) ctxs where
  type
    WorkerT e (Image :> api) m =
      ReadableStream -> WorkerT e api m
  hoistWorkerWithContext pe _ pctx hoist s =
    hoistWorkerWithContext pe (Proxy @api) pctx hoist . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck pure
    where
      ctCheck = withRequest $ \request _ _ -> do
        let cty =
              lookup "Content-Type" $
                map (Bi.first CI.mk) . Req.getHeaders $
                  request.rawRequest
        if maybe False (\ty -> ty == "image/png" || ty == "image/jpeg") cty
          then case fromNullable $ Req.getBody request.rawRequest of
            Just body -> pure body
            Nothing -> delayedFail err400
          else delayedFail err415

adminAPI :: AuthResult User -> AdminAPI (AsWorker HumblrEnv)
adminAPI usr =
  AdminAPI
    { putArticle = putArticle usr
    , postArticle = postArticle usr
    , deleteArticle = deleteArticle usr
    , putImage = putResource usr
    , postImage = putResource usr
    }

putResource :: AuthResult User -> T.Text -> T.Text -> ReadableStream -> Handler HumblrEnv T.Text
putResource user slug name body = protectIfConfigured user $ do
  storage <- getBinding "Storage"
  liftIO $ await' =<< storage.put slug name body

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

listTagArticles :: T.Text -> Maybe Word -> App (Paged Article)
listTagArticles tag mpage = do
  db <- getBinding "Database"
  liftIO $ await' =<< db.listTagArticles tag mpage

listArticles :: Maybe Word -> App (Paged Article)
listArticles mpage = do
  db <- getBinding "Database"
  liftIO $ await' =<< db.listArticles mpage
