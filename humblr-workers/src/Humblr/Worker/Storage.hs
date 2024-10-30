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

module Humblr.Worker.Storage (
  handlers,
  JSObject (..),
  StorageService,
  StorageServiceClass,
  ResourceException (..),
) where

import Control.Exception.Safe (Exception (..), throwIO)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.ReadableStream (ReadableStream)
import Network.Cloudflare.Worker.Binding hiding (getBinding, getSecret)
import Network.Cloudflare.Worker.Binding.R2 (R2Class)
import Network.Cloudflare.Worker.Binding.R2 qualified as R2
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Wasm.Prelude.Linear qualified as PL

type App = ServiceM StorageEnv '[]

data StorageServiceFuns = StorageServiceFuns
  { get :: T.Text -> App (Maybe WorkerResponse)
  , put :: T.Text -> T.Text -> ReadableStream -> App T.Text
  }
  deriving (Generic)
  deriving anyclass (ToService StorageEnv)

type StorageFuns = Signature StorageEnv StorageServiceFuns

type StorageService = Service StorageFuns

type StorageServiceClass = ServiceClass StorageFuns

type StorageEnv =
  BindingsClass
    '[]
    '[]
    '[ '("R2", R2Class)]

handlers :: IO (Service StorageFuns)
handlers =
  toService @StorageEnv StorageServiceFuns {get, put}

data ResourceException = ResourceNotFound T.Text
  deriving (Show, Exception)

get :: T.Text -> App (Maybe WorkerResponse)
get name = do
  r2 <- getBinding "R2"

  liftIO $ runMaybeT do
    objInfo <-
      MaybeT $
        await'
          =<< (R2.head r2 (TE.encodeUtf8 name))
    body <-
      MaybeT $
        mapM R2.getBody
          =<< await'
          =<< R2.get r2 (TE.encodeUtf8 name)
    liftIO do
      hdrs <- Resp.toHeaders mempty
      R2.writeObjectHttpMetadata objInfo hdrs
      ok <- fromHaskellByteString "Ok"
      automatic <- fromHaskellByteString "automatic"
      empty <- emptyObject
      Resp.newResponse'
        (Just $ inject body)
        $ Just
        $ newDictionary
          PL.$ setPartialField "status" (toJSPrim 200)
          PL.. setPartialField "headers" (inject hdrs)
          PL.. setPartialField "statusText" ok
          PL.. setPartialField "encodeBody" automatic
          PL.. setPartialField "cf" empty

put :: T.Text -> T.Text -> ReadableStream -> App T.Text
put slug path body = do
  let name = slug <> "/" <> path
  r2 <- getBinding "R2"
  liftIO do
    void $
      maybe (throwIO $ ResourceNotFound name) pure
        =<< await'
        =<< R2.put r2 (TE.encodeUtf8 name) (nonNull $ inject body)
  pure name
