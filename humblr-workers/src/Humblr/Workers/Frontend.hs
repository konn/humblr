{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Workers.Frontend (frontendHandlers, JSObject (..), JSHandlers) where

import Control.Concurrent.Async (wait)
import Control.Exception (someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Control.Exception.Safe (Exception (..), SomeException (..), handleAny, throwIO, throwString)
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Stack
import GHC.Wasm.Object.Builtins
import GHC.Word
import Humblr.Types (Article (..))
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.Cache qualified as Cache
import Network.Cloudflare.Worker.Binding.D1 (D1, D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
import Network.Cloudflare.Worker.Binding.D1 qualified as D1
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

data TagRow = TagRow {id :: !TagId, name :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

data ArticleTagRow = ArticleTagRow {id :: !Word32, article :: !ArticleId, tag :: !TagId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

data ArticleRow = ArticleRow
  { id :: !ArticleId
  , title :: !T.Text
  , body :: !T.Text
  , createdAt :: !UTCTime
  , lastUpdate :: !UTCTime
  , slug :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

type family xs ~> a where
  '[] ~> a = a
  (x ': xs) ~> a = x -> (xs ~> a)

newtype Preparation params = Preparation (params ~> IO D1.Statement)

bind :: Preparation params -> params ~> IO D1.Statement
bind (Preparation f) = f

data PresetQueries = PresetQueries
  { tryInsertTag :: !(Preparation '[T.Text])
  , lookupTagName :: !(Preparation '[T.Text])
  , articleTags :: !(Preparation '[ArticleId])
  }

newtype TryInsertTagQ = TryInsertTagQ {prepared :: D1.PreparedStatement}

newtype LookupTagNameQ = LookupTagNameQ {prepared :: D1.PreparedStatement}

mkTryInsertTagQ :: D1 -> IO (Preparation '[T.Text])
mkTryInsertTagQ d1 =
  D1.prepare d1 "INSERT OR IGNORE INTO tags (name) VALUES (?)"
    <&> \prep -> Preparation \name ->
      D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkLookupTagNameQ :: D1 -> IO (Preparation '[T.Text])
mkLookupTagNameQ d1 =
  D1.prepare d1 "SELECT * FROM tags WHERE name = ?" <&> \prep ->
    Preparation \name ->
      D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkArticleTagsQ :: D1 -> IO (Preparation '[ArticleId])
mkArticleTagsQ d1 =
  D1.prepare d1 "SELECT tag.name FROM tags tag INNER JOIN articleTags assoc ON tag.id = assoc.tag WHERE assoc.article = ?" <&> \prep ->
    Preparation \aid ->
      D1.bind prep (V.singleton $ D1.toD1ValueView aid)

data AppException = AppException !T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

newtype TagName = TagName {name :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row)

getTagName :: TagName -> T.Text
getTagName = coerce

fromArticleRow :: (HasCallStack) => PresetQueries -> ArticleRow -> IO Article
fromArticleRow qs arow = do
  rows <- wait =<< D1.all =<< bind qs.articleTags arow.id
  unless rows.success $
    throwString "Failed to fetch tags for article"
  let (fails, tags) = partitionEithers $ map (fmap getTagName . D1.parseD1RowView) $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse tag row: " <> show fails
  pure
    Article
      { title = arow.title
      , body = arow.body
      , slug = arow.slug
      , updatedAt = arow.lastUpdate
      , createdAt = arow.createdAt
      , id = arow.id.articleId
      , tags
      }

makeSureTagExists :: (HasCallStack) => PresetQueries -> D1 -> T.Text -> IO TagRow
makeSureTagExists qs d1 tag = do
  rows <-
    wait
      =<< D1.batch d1 . V.fromList
      =<< sequenceA
        [ bind qs.tryInsertTag tag
        , bind qs.lookupTagName tag
        ]
  let resl = V.last rows
  unless resl.success $
    throwString "Failed to lookup tag"
  when (V.null resl.results) $
    throwString "No corresponding tag found!"
  case D1.parseD1RowView $ V.head resl.results of
    Right t -> pure t
    Left err ->
      throwString $
        "Failed to parse tag row: "
          <> show (V.head resl.results)
          <> "\nReason: "
          <> err
