{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Workers.Frontend (frontendHandlers, JSObject (..), JSHandlers) where

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
import GHC.Word
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.Cache qualified as Cache
import Network.Cloudflare.Worker.Binding.D1 (D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
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

frontend :: (HasCallStack) => Frontend
frontend req env ctx = handleAny reportError do
  meth <- CI.mk <$> toHaskellByteString (Req.getMethod req)
  unless (meth == "GET") $ throwCode 405 "Method Not Allowed"
  let uri = fromJust $ parseURI $ T.unpack $ Req.getUrl req
      rawPathInfo = BS8.pack uri.uriPath
      pathInfo = decodePathSegments rawPathInfo
      objPath = BS8.dropWhile (== '/') rawPathInfo
  mcache <- fmap fromNullable . await =<< Cache.match (inject req) Nothing
  case mcache of
    Just rsp -> do
      pure rsp
    Nothing -> do
      let r2 = getBinding "R2" env
      objBody <-
        maybe (throwCode 404 $ "404 Not Found: " <> TE.decodeUtf8 objPath) pure
          =<< wait
          =<< R2.get r2 objPath
      src <- R2.getBody objBody
      let etag = R2.getObjectHTTPETag objBody
          ctype = defaultMimeLookup $ last pathInfo
      hdrs <-
        Resp.toHeaders $
          Map.fromList
            [ ("ETag", etag)
            , ("Cache-Control", "public, max-age=3600")
            , ("Content-Type", ctype)
            ]
      empty <- emptyObject
      resp <-
        Resp.newResponse' (Just $ inject src) $
          Just $
            newDictionary
              ( setPartialField "statusText" (fromBS "OK")
                  PL.. setPartialField "status" (toJSPrim 200)
                  PL.. setPartialField "headers" (inject hdrs)
                  PL.. setPartialField "encodeBody" (fromBS "automatic")
                  PL.. setPartialField "cf" empty
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
  let (stt, msg) = case exc of
        (fromException -> Just (ResponseError code m)) -> (code, m)
        _ -> (500, "Unknown Exception: " <> T.pack (displayException exc))
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

newtype TagId = TagId {tagId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

newtype ArticleId = ArticleId {articleId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

data Tag = Tag {id :: !TagId, name :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

data ArticleTag = ArticleTag {articleId :: !ArticleId, tagId :: !TagId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

data Article = Article {id :: !ArticleId, title :: !T.Text, body :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)
