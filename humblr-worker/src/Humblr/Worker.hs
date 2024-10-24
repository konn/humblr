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

module Humblr.Worker (handlers, JSObject (..), JSHandlers) where

import Control.Concurrent.Async (wait)
import Control.Exception.Safe (Exception (..), throwString)
import Control.Monad
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful hiding (inject, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Servant.Cloudflare.Workers
import Effectful.Servant.Cloudflare.Workers.Assets (
  AssetsClass,
  serveAssets,
 )
import Effectful.Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCached)
import Effectful.Servant.Cloudflare.Workers.D1 qualified as D1
import Effectful.Servant.Cloudflare.Workers.R2 (R2Class)
import Effectful.Servant.Cloudflare.Workers.R2 qualified as R2
import GHC.Generics (Generic)
import GHC.Stack
import GHC.Wasm.Object.Builtins
import GHC.Word
import Humblr.Types
import Network.Cloudflare.Worker.Binding
import Network.Cloudflare.Worker.Binding qualified as B
import Network.Cloudflare.Worker.Binding.Assets qualified as RawAssets
import Network.Cloudflare.Worker.Binding.D1 (D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Request qualified as Req
import Network.URI
import Servant.API (Raw, toUrlPiece)
import Servant.Auth.Cloudflare.Workers (defaultCloudflareZeroTrustSettings, toJWTSettings)
import Servant.Cloudflare.Workers.Cache (serveCachedRaw)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude (Tagged (..), WorkerT)

type HumblrEnv =
  BindingsClass
    '["BASE_URL", "CF_TEAM_NAME"]
    '["CF_AUD_TAG"]
    '[ '("R2", R2Class)
     , '("D1", D1Class)
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
  genericCompileWorkerContextWith @HumblrEnv
    id
    ( \env _ -> do
        let audience = do
              let aud = B.getSecret "CF_AUD_TAG" env
              guard $ not $ T.null aud
              pure aud
        team0 <- case A.fromJSON $ B.getEnv "CF_TEAM_NAME" env of
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

workers ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  RootAPI (AsWorkerT HumblrEnv (Eff es))
workers =
  RootAPI
    { frontend = frontend
    , assets = serveCachedRaw assetCacheOptions $ serveAssets "ASSETS"
    , apiRoutes = apiRoutes
    , resources = resources
    }

resources :: WorkerT HumblrEnv Raw (Eff es)
resources = R2.serveBucketRel "R2"

frontend :: FrontendRoutes (AsWorkerT HumblrEnv (Eff es))
frontend =
  FrontendRoutes
    { topPage = const serveIndex
    , tagArticles = const $ const serveIndex
    , newArticle = serveIndex
    , editArticle = const serveIndex
    , articlePage = const serveIndex
    }

serveIndex :: WorkerT HumblrEnv Raw (Eff es)
serveIndex = serveCachedRaw assetCacheOptions $ Tagged \req env _ ->
  if not $ null req.pathInfo
    then toWorkerResponse $ responseServerError err404 {errBody = "Not Found"}
    else do
      let link = "/" <> toUrlPiece rootApiLinks.assets <> "/index.html"
          rawUrl = Req.getUrl req.rawRequest
          !url = fromString @USVString $ show $ (fromMaybe (error $ "Invalid Url: " <> show rawUrl) $ parseURI $ T.unpack rawUrl) {uriPath = T.unpack link}
      resp <- await =<< RawAssets.fetch (B.getBinding "ASSETS" env) (inject url)
      pure resp

apiRoutes ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  AdminAPI (AsWorkerT HumblrEnv (Eff es))
apiRoutes =
  AdminAPI
    { -- putArticle = _putArticle
      -- , postArticle = _postArticle
      -- , listTags = _listTags
      listTagArticles = listTagArticles
    , listArticles = listArticles
    , -- , headArticle = _headArticle
      getArticle = getArticle
      -- , deleteArticle = _deleteArticle
    }

getArticle :: (HasUniqueWorkerWith HumblrEnv es) => T.Text -> Eff es Article
getArticle slug = do
  serveCached
    CacheOptions
      { cacheTTL = 3600 * 24
      , onlyOk = True
      , includeQuery = True
      }
  qs <- getPresetQueries
  maybe
    ( serverError
        err404
          { errBody =
              "Article Not Found: "
                <> LBS.fromStrict (TE.encodeUtf8 slug)
          }
    )
    pure
    =<< lookupSlug qs slug

listTagArticles :: (HasUniqueWorkerWith HumblrEnv es) => T.Text -> Maybe Word -> Eff es [Article]
listTagArticles tag mpage = do
  qs <- getPresetQueries
  getArticlesWithTag qs tag $ fromIntegral <$> mpage

listArticles ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Maybe Word ->
  Eff es [Article]
listArticles mpage = do
  qs <- getPresetQueries
  getRecentArticles qs $ fromIntegral <$> mpage

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

newtype Preparation params = Preparation (forall es -> (HasUniqueWorker es) => params ~> Eff es D1.Statement)

bind :: forall es params. Preparation params -> (HasUniqueWorker es) => params ~> Eff es D1.Statement
bind (Preparation f) =
  f es

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

getPresetQueries :: (HasUniqueWorkerWith HumblrEnv es) => Eff es PresetQueries
getPresetQueries = do
  tryInsertTag <- mkTryInsertTagQ
  lookupTagName <- mkLookupTagNameQ
  articleTags <- mkArticleTagsQ
  insertArticle <- mkInsertArticleQ
  tagArticle <- mkTagArticleQ
  articleWithTags <- mkArticleWithTagsQ
  lookupFromSlug <- mkLookupSlugQ
  recentArticles <- mkRecentArticlesQ
  pure PresetQueries {..}

getRecentArticles ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  PresetQueries ->
  Maybe Word32 ->
  Eff es [Article]
getRecentArticles qs page = do
  rows <-
    unsafeEff_ . wait
      =<< D1.all
      =<< bind qs.recentArticles (maybe 0 (* 10) page)
  unless rows.success $
    throwString "Failed to fetch recent articles"
  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM (fromArticleRow qs) articles

mkRecentArticlesQ ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Eff es (Preparation '[Word32])
mkRecentArticlesQ =
  D1.prepare' "D1" "SELECT * FROM articles ORDER BY createdAt DESC LIMIT 10 OFFSET ?" <&> \prep ->
    Preparation \_ page ->
      D1.bind prep (V.singleton $ D1.toD1ValueView $ page * 10)

mkLookupSlugQ :: (HasUniqueWorkerWith HumblrEnv es) => Eff es (Preparation '[T.Text])
mkLookupSlugQ =
  D1.prepare' "D1" "SELECT * FROM articles WHERE slug = ?" <&> \prep ->
    Preparation \_ slug ->
      D1.bind prep (V.singleton $ D1.toD1ValueView slug)

lookupSlug ::
  ( HasCallStack
  , HasUniqueWorkerWith HumblrEnv es
  ) =>
  PresetQueries ->
  T.Text ->
  Eff es (Maybe Article)
lookupSlug qs slug = do
  mrow <-
    unsafeEff_ . wait
      =<< D1.first
      =<< bind qs.lookupFromSlug slug
  forM mrow $ \row -> do
    case D1.parseD1RowView row of
      Right r -> fromArticleRow qs r
      Left err -> throwString err

mkTryInsertTagQ :: (HasUniqueWorkerWith HumblrEnv es) => Eff es (Preparation '[T.Text])
mkTryInsertTagQ =
  D1.prepare' "D1" "INSERT OR IGNORE INTO tags (name) VALUES (?)"
    <&> \prep -> Preparation \_ name ->
      D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkLookupTagNameQ ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Eff es (Preparation '[T.Text])
mkLookupTagNameQ =
  D1.prepare' "D1" "SELECT * FROM tags WHERE name = ?" <&> \prep ->
    Preparation \_ name ->
      D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkArticleTagsQ ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Eff es (Preparation '[ArticleId])
mkArticleTagsQ =
  D1.prepare' "D1" "SELECT tag.name FROM tags tag INNER JOIN articleTags assoc ON tag.id = assoc.tag WHERE assoc.article = ?" <&> \prep ->
    Preparation \_ aid ->
      D1.bind prep (V.singleton $ D1.toD1ValueView aid)

mkInsertArticleQ ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Eff es (Preparation '[Article])
mkInsertArticleQ =
  D1.prepare' "D1" "INSERT INTO articles (body, createdAt, lastUpdate, slug) VALUES (?1, ?2, ?3, ?4, ?5)" <&> \prep ->
    Preparation \_ Article {..} ->
      D1.bind prep $
        V.fromList
          [ D1.toD1ValueView body
          , D1.toD1ValueView createdAt
          , D1.toD1ValueView updatedAt
          , D1.toD1ValueView slug
          ]

mkTagArticleQ ::
  (HasUniqueWorkerWith HumblrEnv es) =>
  Eff es (Preparation '[ArticleId, TagId])
mkTagArticleQ =
  D1.prepare' "D1" "INSERT INTO articleTags (article, tag) VALUES (?1, ?2)" <&> \prep ->
    Preparation \_ aid tid ->
      D1.bind prep $
        V.fromList
          [ D1.toD1ValueView aid
          , D1.toD1ValueView tid
          ]

mkArticleWithTagsQ :: (HasUniqueWorkerWith HumblrEnv es) => Eff es (Preparation '[T.Text, Word32])
mkArticleWithTagsQ =
  D1.prepare' "D1" "SELECT a.* FROM articles a INNER JOIN articleTags at ON a.id = at.article INNER JOIN tags t ON at.tag = t.id WHERE t.name = ?1 ORDER BY a.createdAt DESC LIMIT 10 OFFSET ?2" <&> \prep ->
    Preparation \_ name page ->
      D1.bind prep (V.fromList [D1.toD1ValueView name, D1.toD1ValueView $ page * 10])

getArticlesWithTag :: (HasCallStack, HasUniqueWorker es) => PresetQueries -> T.Text -> Maybe Word32 -> Eff es [Article]
getArticlesWithTag qs tag mpage = do
  rows <-
    unsafeEff_ . wait
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

fromArticleRow ::
  (HasCallStack, HasUniqueWorker es) =>
  PresetQueries ->
  ArticleRow ->
  Eff es Article
fromArticleRow qs arow = do
  rows <- unsafeEff_ . wait =<< D1.all =<< bind qs.articleTags arow.id
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
