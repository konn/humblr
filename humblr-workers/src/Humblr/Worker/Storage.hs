{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Worker.Storage (
  handlers,
  JSObject (..),
  StorageService,
  StorageServiceClass,
  ResourceException (..),
  GetParams (..),
  SignParams (..),
) where

import Control.Exception.Safe (Exception (..), throwIO, throwString)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Type.Equality
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.SubtleCrypto (js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any, js_fun_sign_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any)
import GHC.Wasm.Web.JSON (encodeJSON)
import GHC.Wasm.Web.ReadableStream (ReadableStream)
import Humblr.Types (RootAPI (..), rootApiLinks)
import Language.WASM.JSVal.Convert
import Network.Cloudflare.Worker.Binding hiding (getBinding, getEnv, getSecret)
import Network.Cloudflare.Worker.Binding.KV (KVClass)
import Network.Cloudflare.Worker.Binding.KV qualified as KV
import Network.Cloudflare.Worker.Binding.R2 (R2Class)
import Network.Cloudflare.Worker.Binding.R2 qualified as R2
import Network.Cloudflare.Worker.Binding.Service (
  FunSig (..),
  IsServiceArg,
  Service,
  ServiceClass,
  ServiceM,
  ToService (..),
  getBinding,
  getEnv,
  type (~>),
 )
import Network.Cloudflare.Worker.Crypto (subtleCrypto)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Servant.Auth.Cloudflare.Workers.Internal.JWT (CryptoKey, JWSAlg (HS256), toAlogirhtmIdentifier, verifySignature)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (ServerError (..), err403, responseServerError)
import Servant.Cloudflare.Workers.Prelude (ToHttpApiData (toUrlPiece))
import Wasm.Prelude.Linear qualified as PL

type App = ServiceM StorageEnv '[]

data StorageServiceFuns = StorageServiceFuns
  { get :: GetParams -> App (Maybe WorkerResponse)
  , put :: T.Text -> T.Text -> ReadableStream -> App T.Text
  , issueSignedURL :: SignParams -> App (Maybe T.Text)
  }
  deriving (Generic)
  deriving anyclass (ToService StorageEnv)

type StorageFuns = Signature StorageEnv StorageServiceFuns

type StorageService = Service StorageFuns

type StorageServiceClass = ServiceClass StorageServiceFields

type StorageServiceFields =
  '[ '("get", GetParams ~> Return (Maybe WorkerResponse))
   , '("put", T.Text ~> T.Text ~> ReadableStream ~> Return T.Text)
   , '("issueSignedURL", SignParams ~> Return (Maybe T.Text))
   ]

_ = Refl :: StorageServiceFields :~: Signature StorageEnv StorageServiceFuns

type StorageEnv =
  BindingsClass
    '["ROOT_URI"]
    '[]
    '[ '("R2", R2Class), '("KV", KVClass)]

data GetParams = GetParams
  { name :: !T.Text
  , expiry :: !POSIXTime
  , sign :: !T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (IsServiceArg) via ViaJSON GetParams

data SignParams = SignParams {name :: T.Text, duration :: !Word}
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (IsServiceArg) via ViaJSON SignParams

data SignPayload = SignPayload {name :: !T.Text, expiry :: !POSIXTime}
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (IsServiceArg) via ViaJSON SignPayload

handlers :: IO (Service StorageFuns)
handlers =
  toService @StorageEnv StorageServiceFuns {get, put, issueSignedURL}

data ResourceException = ResourceNotFound T.Text
  deriving (Show, Exception)

issueSignedURL :: SignParams -> App (Maybe T.Text)
issueSignedURL SignParams {..} = do
  r2 <- getBinding "R2"
  key <- getSignKey
  root <- getEnv "ROOT_URI"
  liftIO $ runMaybeT do
    void $ MaybeT $ await' =<< R2.head r2 (TE.encodeUtf8 name)
    liftIO do
      now <- getPOSIXTime
      let expiry = fromIntegral $ ceiling @_ @Int $ now + fromIntegral duration
      let payload = A.encode $ SignPayload {..}
      sgn <- useByteStringAsJSByteArray @Word8 (LBS.toStrict payload) \bs ->
        fmap (toByteString @Word8) . fromArrayBuffer . unsafeCast
          =<< await
          =<< js_fun_sign_AlgorithmIdentifier_CryptoKey_BufferSource_Promise_any
            subtleCrypto
            (toAlogirhtmIdentifier HS256)
            key
            (inject bs)
      let sgnBS = B64U.encodeUnpadded sgn
          signedUrl =
            T.dropWhileEnd (== '/') root
              <> "/"
              <> toUrlPiece (rootApiLinks.resources (T.splitOn "/" name) expiry $ TE.decodeUtf8 sgnBS)
      pure signedUrl

getSignKey :: App CryptoKey
getSignKey = do
  kv <- getBinding "KV"
  liftIO do
    jwk <-
      either
        (throwString . ("Invalid JWT: " <>))
        (fmap unsafeCast . encodeJSON)
        . A.eitherDecodeStrict @A.Value
        . BS8.pack
        =<< maybe (throwString "No URL_SIGN_KEY set!") pure
        =<< KV.get kv "URL_SIGN_KEY"
    fmap unsafeCast . await
      =<< js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
        subtleCrypto
        "jwk"
        (upcast jwk)
        (toAlogirhtmIdentifier HS256)
        False
        (toSequence $ V.fromList ["verify", "sign"])

newtype Sign = Sign {runSign :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsServiceArg)

get :: GetParams -> App (Maybe WorkerResponse)
get GetParams {..} = do
  r2 <- getBinding "R2"
  key <- getSignKey
  liftIO $ runMaybeT do
    now <- liftIO getPOSIXTime
    let payload = A.encode SignPayload {..}
        goodSign =
          verifySignature
            key
            (B64U.decodeLenient $ TE.encodeUtf8 sign)
            (LBS.toStrict payload)
    if
      | not goodSign -> do
          liftIO $ toWorkerResponse $ responseServerError err403 {errBody = "Invalid Signature"}
      | now >= expiry -> do
          liftIO $ toWorkerResponse $ responseServerError err403 {errBody = "URL already expired"}
      | otherwise -> do
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
