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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Workers.Frontend (frontendHandlers, JSObject (..), JSHandlers) where

import CMark qualified as CM
import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Control.Exception.Safe (Exception (..), SomeException (..), handleAny, throwIO, throwString)
import Control.Lens ((.~))
import Control.Monad
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Time (TimeZone (..), UTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Stack
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.Headers qualified as Headers
import GHC.Word
import Humblr.Html (PageOptions (..), RenderingOptions (..), articlePage, articleTable, getSummary, nodeToPlainText, toStandaloneHtml)
import Humblr.Types (Article (..))
import Lucid qualified as H
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding.Cache qualified as Cache
import Network.Cloudflare.Worker.Binding.D1 (D1, D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
import Network.Cloudflare.Worker.Binding.D1 qualified as D1
import Network.Cloudflare.Worker.Binding.R2 (R2Class)
import Network.Cloudflare.Worker.Binding.R2 qualified as R2
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext, FetchHandler, waitUntil)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response qualified as Resp
import Network.HTTP.Types.URI (decodePathSegments, encodePathSegmentsRelative, parseSimpleQuery)
import Network.Mime (defaultMimeLookup)
import Network.URI
import Text.Read (readMaybe)
import Wasm.Prelude.Linear qualified as PL

type FrontendEnv =
  BindingsClass
    '["BASE_URL"]
    '[]
    '[ '("R2", R2Class)
     , '("D1", D1Class)
     ]

type Frontend = FetchHandler FrontendEnv

frontendHandlers :: IO JSHandlers
frontendHandlers = toJSHandlers Handlers {fetch = frontend}

getPathOrIndex :: R2.R2 -> BS.ByteString -> IO (Async (Maybe R2.R2ObjectBody))
getPathOrIndex r2 key
  | "/" `BS8.isSuffixOf` key = R2.get r2 $ key <> "index.html"
  | otherwise = do
      future <- R2.get r2 key
      async do
        mobj <- wait future
        case mobj of
          Just a -> pure $ Just a
          Nothing ->
            wait =<< R2.get r2 (BS8.dropWhileEnd (== '/') key <> "/index.html")

frontend :: (HasCallStack) => Frontend
frontend req env ctx = handleAny reportError do
  meth <- CI.mk <$> toHaskellByteString (Req.getMethod req)
  unless (meth == "GET") $ throwCode 405 "Method Not Allowed"
  let uri = fromJust $ parseURI $ T.unpack $ Req.getUrl req
      rawPathInfo = BS8.pack uri.uriPath
      pathInfo = decodePathSegments rawPathInfo
  case pathInfo of
    [] ->
      serveCached
        CacheOptions
          { cacheTTL = 1200 * 24
          , onlyOk = True
          , includeQuery = True
          }
        req
        ctx
        uri
        $ serveTopPage env ctx
    ["tag", t] ->
      serveCached
        CacheOptions
          { cacheTTL = 3600 * 24
          , onlyOk = True
          , includeQuery = True
          }
        req
        ctx
        uri
        $ serveTagPage env ctx uri t
    ["article", slug] ->
      serveCached
        CacheOptions
          { cacheTTL = 3600 * 24
          , onlyOk = True
          , includeQuery = True
          }
        req
        ctx
        uri
        $ serveArticle req env ctx slug
    "static" : rest ->
      serveCached
        CacheOptions
          { cacheTTL = 3600 * 24 * 31
          , onlyOk = True
          , includeQuery = True
          }
        req
        ctx
        uri
        $ serveStatic
          req
          env
          ctx
          (LBS.toStrict $ BB.toLazyByteString $ encodePathSegmentsRelative rest)
          rest
    _ -> throwCode 404 $ "Not Found: " <> TE.decodeUtf8 rawPathInfo

data CacheOptions = CacheOptions
  { cacheTTL :: !Word32
  , onlyOk :: !Bool
  , includeQuery :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

serveCached ::
  CacheOptions ->
  Req.WorkerRequest ->
  FetchContext ->
  URI ->
  IO Resp.WorkerResponse ->
  IO Resp.WorkerResponse
serveCached opts req ctx uri act = do
  let cachePath =
        T.pack $
          show $
            uri
              & #uriFragment .~ ""
              & if opts.includeQuery then id else #uriQuery .~ ""
  reqHdrs0 <- Resp.toHeaders $ Map.fromList (Req.getHeaders req)
  keyReq <-
    Req.newRequest (Just cachePath) $
      Just $
        newDictionary
          PL.$ setPartialField "headers" (upcast reqHdrs0)
  mcache <- fmap fromNullable . await =<< Cache.match (inject keyReq) Nothing
  case mcache of
    Just resp -> pure resp
    Nothing -> do
      resp <- act
      code <- Resp.getStatus resp
      when (not opts.onlyOk && code == 200) do
        respHdrs0 <- Resp.getHeaders resp
        cacheControlHdr <- fromHaskellByteString "Cache-Control"
        cacheControl <-
          fromHaskellByteString $
            "public, max-age=" <> BS8.pack (show opts.cacheTTL)
        Headers.js_fun_set_ByteString_ByteString_undefined
          respHdrs0
          cacheControlHdr
          cacheControl
        Resp.setHeaders resp respHdrs0
        waitUntil ctx =<< Cache.put keyReq resp
      pure resp

serveTopPage :: JSObject FrontendEnv -> FetchContext -> IO Resp.WorkerResponse
serveTopPage env _ctx = do
  let d1 = getBinding "D1" env
  qs <- getPresetQueries d1
  arts <- getRecentArticles qs Nothing
  let body =
        LT.toStrict
          $ H.renderText
          $ toStandaloneHtml
            PageOptions {siteName = "ごはんぶらー", title = "ごはんぶらー"}
          $ articleTable (toRenderingOpts $ getEnv "BASE_URL" env) arts
  Resp.newResponse
    Resp.SimpleResponseInit
      { status = 200
      , statusText = "OK"
      , body = body
      , headers = Map.fromList [("Content-Type", "text/html")]
      }

serveArticle ::
  (HasCallStack) =>
  Req.WorkerRequest ->
  JSObject FrontendEnv ->
  FetchContext ->
  T.Text ->
  IO Resp.WorkerResponse
serveArticle _req env _ctx slug = do
  let d1 = getBinding "D1" env
      opts = toRenderingOpts $ getEnv "BASE_URL" env
  qs <- getPresetQueries d1
  art <- maybe (throwCode 404 $ "Article Not Found: " <> slug) pure =<< lookupSlug qs slug
  let !title =
        maybe "記事" (T.take 20 . T.strip . nodeToPlainText) $
          getSummary $
            CM.commonmarkToNode [] art.body
      !body =
        LT.toStrict
          $ H.renderText
          $ toStandaloneHtml
            PageOptions
              { siteName = "ごはんぶらー"
              , title
              }
          $ articlePage opts art
  Resp.newResponse
    Resp.SimpleResponseInit
      { status = 200
      , statusText = "OK"
      , body = body
      , headers = Map.fromList [("Content-Type", "text/html")]
      }

toRenderingOpts :: J.Value -> RenderingOptions
toRenderingOpts (J.String dom) =
  RenderingOptions
    { imageBase = dom <> "/" <> "static"
    , articleBase = dom <> "/" <> "article"
    , tagBase = dom <> "/" <> "tag"
    , timeZone = jst
    }
toRenderingOpts _ =
  RenderingOptions
    { imageBase = "../static"
    , articleBase = "../article"
    , tagBase = "../tag"
    , timeZone = jst
    }

jst :: TimeZone
jst = TimeZone {timeZoneSummerOnly = False, timeZoneName = "JST", timeZoneMinutes = 9 * 60}

serveTagPage ::
  JSObject FrontendEnv ->
  FetchContext ->
  URI ->
  T.Text ->
  IO Resp.WorkerResponse
serveTagPage env _ctx uri tag = do
  let d1 = getBinding "D1" env
      mpage = do
        mraw <- lookup "page" $ parseSimpleQuery $ BS8.pack $ uri.uriQuery
        readMaybe $ BS8.unpack mraw
  qs <- getPresetQueries d1
  arts <- getArticlesWithTag qs tag mpage
  let body =
        LT.toStrict
          $ H.renderText
          $ toStandaloneHtml
            PageOptions {siteName = "ごはんぶらー", title = "Articles for #" <> tag}
          $ articleTable (toRenderingOpts $ getEnv "BASE_URL" env) arts
  Resp.newResponse
    Resp.SimpleResponseInit
      { status = 200
      , statusText = "OK"
      , body = body
      , headers = Map.fromList [("Content-Type", "text/html")]
      }

serveStatic :: Req.WorkerRequest -> JSObject FrontendEnv -> FetchContext -> BS8.ByteString -> [T.Text] -> IO Resp.WorkerResponse
serveStatic _req env _ctx rawPathInfo pathInfo = do
  let objPath = BS8.dropWhile (== '/') rawPathInfo
      r2 = getBinding "R2" env
  objBody <-
    maybe (throwCode 404 $ "404 Not Found: " <> TE.decodeUtf8 objPath) pure
      =<< wait
      =<< getPathOrIndex r2 objPath
  src <- R2.getBody objBody
  let etag = R2.getObjectHTTPETag objBody
      ctype = defaultMimeLookup $ last pathInfo
  hdrs <- Resp.toHeaders mempty
  R2.writeObjectHttpMetadata objBody hdrs
  let cacheHdrs =
        [ ("ETag", etag)
        , ("Content-Type", ctype)
        ]
  forM_ cacheHdrs $ \(k, v) -> do
    k' <- fromHaskellByteString k
    v' <- fromHaskellByteString v
    Headers.js_fun_set_ByteString_ByteString_undefined hdrs k' v'
  empty <- emptyObject
  Resp.newResponse' (Just $ inject src) $
    Just $
      newDictionary
        ( setPartialField "statusText" (fromBS "OK")
            PL.. setPartialField "status" (toJSPrim 200)
            PL.. setPartialField "headers" (inject hdrs)
            PL.. setPartialField "encodeBody" (fromBS "automatic")
            PL.. setPartialField "cf" empty
        )

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
  , insertArticle :: !(Preparation '[Article])
  , tagArticle :: !(Preparation '[ArticleId, TagId])
  , articleWithTags :: !(Preparation '[T.Text, Word32])
  , lookupFromSlug :: !(Preparation '[T.Text])
  , recentArticles :: !(Preparation '[Word32])
  }

getPresetQueries :: D1 -> IO PresetQueries
getPresetQueries d1 = do
  tryInsertTag <- mkTryInsertTagQ d1
  lookupTagName <- mkLookupTagNameQ d1
  articleTags <- mkArticleTagsQ d1
  insertArticle <- mkInsertArticleQ d1
  tagArticle <- mkTagArticleQ d1
  articleWithTags <- mkArticleWithTagsQ d1
  lookupFromSlug <- mkLookupSlugQ d1
  recentArticles <- mkRecentArticlesQ d1
  pure PresetQueries {..}

getRecentArticles :: PresetQueries -> Maybe Word32 -> IO [Article]
getRecentArticles qs page = do
  rows <-
    wait
      =<< D1.all
      =<< bind qs.recentArticles (maybe 0 (* 10) page)
  unless rows.success $
    throwString "Failed to fetch recent articles"
  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM (fromArticleRow qs) articles

mkRecentArticlesQ :: D1 -> IO (Preparation '[Word32])
mkRecentArticlesQ d1 =
  D1.prepare d1 "SELECT * FROM articles ORDER BY createdAt DESC LIMIT 10 OFFSET ?" <&> \prep ->
    Preparation \page ->
      D1.bind prep (V.singleton $ D1.toD1ValueView $ page * 10)

mkLookupSlugQ :: D1 -> IO (Preparation '[T.Text])
mkLookupSlugQ d1 =
  D1.prepare d1 "SELECT * FROM articles WHERE slug = ?" <&> \prep ->
    Preparation \slug ->
      D1.bind prep (V.singleton $ D1.toD1ValueView slug)

lookupSlug :: (HasCallStack) => PresetQueries -> T.Text -> IO (Maybe Article)
lookupSlug qs slug = do
  mrow <-
    wait
      =<< D1.first
      =<< bind qs.lookupFromSlug slug
  forM mrow $ \row -> do
    case D1.parseD1RowView row of
      Right r -> fromArticleRow qs r
      Left err -> throwString err

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

mkInsertArticleQ :: D1 -> IO (Preparation '[Article])
mkInsertArticleQ d1 =
  D1.prepare d1 "INSERT INTO articles (body, createdAt, lastUpdate, slug) VALUES (?1, ?2, ?3, ?4, ?5)" <&> \prep ->
    Preparation \Article {..} ->
      D1.bind prep $
        V.fromList
          [ D1.toD1ValueView body
          , D1.toD1ValueView createdAt
          , D1.toD1ValueView updatedAt
          , D1.toD1ValueView slug
          ]

mkTagArticleQ :: D1 -> IO (Preparation '[ArticleId, TagId])
mkTagArticleQ d1 =
  D1.prepare d1 "INSERT INTO articleTags (article, tag) VALUES (?1, ?2)" <&> \prep ->
    Preparation \aid tid ->
      D1.bind prep $
        V.fromList
          [ D1.toD1ValueView aid
          , D1.toD1ValueView tid
          ]

mkArticleWithTagsQ :: D1 -> IO (Preparation '[T.Text, Word32])
mkArticleWithTagsQ d1 =
  D1.prepare d1 "SELECT a.* FROM articles a INNER JOIN articleTags at ON a.id = at.article INNER JOIN tags t ON at.tag = t.id WHERE t.name = ?1 ORDER BY a.createdAt DESC LIMIT 10 OFFSET ?2" <&> \prep ->
    Preparation \name page ->
      D1.bind prep (V.fromList [D1.toD1ValueView name, D1.toD1ValueView $ page * 10])

getArticlesWithTag :: (HasCallStack) => PresetQueries -> T.Text -> Maybe Word32 -> IO [Article]
getArticlesWithTag qs tag mpage = do
  rows <-
    wait
      =<< D1.all
      =<< bind qs.articleWithTags tag (fromMaybe 0 mpage)
  unless rows.success $
    throwString "Failed to fetch articles with tag"

  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM (fromArticleRow qs) articles

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
      { body = arow.body
      , slug = arow.slug
      , updatedAt = arow.lastUpdate
      , createdAt = arow.createdAt
      , tags
      }

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSString -> IO ()
