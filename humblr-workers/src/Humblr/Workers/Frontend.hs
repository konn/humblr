{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Humblr.Workers.Frontend (frontendHandlers) where

import Control.Concurrent.Async (wait)
import Control.Exception (someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Control.Exception.Safe (Exception (..), SomeException (..), handleAny, throwIO)
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word16)
import GHC.Generics (Generic)
import GHC.Stack
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.Cache qualified as Cache
import Network.Cloudflare.Worker.Binding.D1 (D1Class)
import Network.Cloudflare.Worker.Binding.R2 (R2Class)
import Network.Cloudflare.Worker.Binding.R2 qualified as R2
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch (FetchHandler, waitUntil)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response qualified as Resp
import Network.HTTP.Types.URI (decodePathSegments)
import Network.Mime (defaultMimeLookup)
import Network.URI
import Wasm.Prelude.Linear qualified as PL

type FrontendEnv =
  BindingsClass
    '[]
    '[]
    '[ '("R2", R2Class)
     , '("D1", D1Class)
     ]

type Frontend = FetchHandler FrontendEnv

frontendHandlers :: IO JSHandlers
frontendHandlers = toJSHandlers Handlers {fetch = frontend}

frontend :: Frontend
frontend req env ctx = handleAny reportError do
  meth <- CI.mk <$> toHaskellByteString (Req.getMethod req)
  unless (meth == "GET") $ throwCode 405 "Method Not Allowed"
  let uri = fromJust $ parseURI $ T.unpack $ Req.getUrl req
      rawPathInfo = BS8.pack uri.uriPath
      pathInfo = decodePathSegments rawPathInfo
      ctype = defaultMimeLookup $ last pathInfo
  rawPath <- fromHaskellByteString rawPathInfo
  mcache <- fmap fromNullable . await =<< Cache.match (inject rawPath) Nothing
  case mcache of
    Just rsp -> pure rsp
    Nothing -> do
      let r2 = getBinding "R2" env
      objBody <-
        maybe (throwCode 404 $ "Not Found: " <> TE.decodeUtf8 rawPathInfo) pure
          =<< wait
          =<< R2.get r2 rawPathInfo
      src <- R2.getBody objBody
      let etag = R2.getObjectHTTPETag objBody
      hdrs <-
        Resp.toHeaders $
          Map.fromList
            [ ("ETag", etag)
            , ("Cache-Control", "public, max-age=3600")
            , ("Content-Type", ctype)
            ]
      resp <-
        Resp.newResponse' (Just $ inject src) $
          Just $
            newDictionary
              ( setPartialField "statusText" (fromBS "OK")
                  PL.. setPartialField "status" (toJSPrim 200)
                  PL.. setPartialField "headers" (inject hdrs)
                  PL.. setPartialField "encodeBody" (fromBS "automatic")
                  PL.. setPartialField "cf" (upcast jsNull)
              )
      waitUntil ctx =<< Cache.put req resp
      pure resp

fromBS :: BS.ByteString -> JSObject JSByteStringClass
{-# NOINLINE fromBS #-}
fromBS = unsafeDupablePerformIO . fromHaskellByteString

data ResponseError = ResponseError !Word16 !T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

throwCode :: (HasCallStack) => Word16 -> T.Text -> IO a
throwCode = fmap throwIO . ResponseError

reportError :: SomeException -> IO Resp.WorkerResponse
reportError exc = do
  let (stt, msg) = case fromException exc of
        Just (ResponseError code m) -> (code, m)
        Nothing -> (500, "Unknown Exception: " <> T.pack (displayException exc))
      body =
        T.unlines
          [ msg
          , ""
          , T.pack $ displayExceptionContext $ someExceptionContext exc
          ]
  Resp.newResponse
    Resp.SimpleResponseInit
      { Resp.status = stt
      , Resp.statusText = ""
      , Resp.body = body
      , Resp.headers = mempty
      }
