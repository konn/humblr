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
import Control.Exception.Safe (Exception (..), throwString, tryAny)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Word
import Humblr.Types
import Network.Cloudflare.Worker.Binding hiding (getBinding, getSecret)
import Network.Cloudflare.Worker.Binding qualified as Raw
import Network.Cloudflare.Worker.Binding.Assets (AssetsClass)
import Network.Cloudflare.Worker.Binding.Assets qualified as RawAssets
import Network.Cloudflare.Worker.Binding.D1 (D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
import Network.Cloudflare.Worker.Binding.D1 qualified as D1
import Network.Cloudflare.Worker.Binding.R2 (R2Class)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.URI
import Servant.Auth.Cloudflare.Workers
import Servant.Cloudflare.Workers.Assets (serveAssets)
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCachedRaw)
import Servant.Cloudflare.Workers.Generic (AsWorker, genericCompileWorkerContext)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude hiding (inject)
import Servant.Cloudflare.Workers.R2 qualified as R2

type App = Handler HumblrEnv

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
resources = R2.serveBucketRel "R2"

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
  art <-
    either (\e -> serverError err500 {errBody = "Schema Error (article): " <> BS8.pack e}) (pure . (.id))
      . D1.parseD1RowView @ArticleRow
      =<< maybe (serverError err404 {errBody = "Article not found: " <> TE.encodeUtf8 slug}) (pure)
      =<< liftIO . (wait <=< D1.first)
      =<< lookupSlugQ slug
  -- FIXME: make these two together with @art@ into a single batch.
  let tags = V.fromList upd.tags
  insTagsQ <- tryInsertTagQ tags
  tagIdsQ <- lookupTagsQ tags
  d1 <- getBinding "D1"
  tagIds <-
    V.mapMaybe (either (const Nothing) (Just . (.id)) . D1.parseD1RowView @TagRow)
      . V.concatMap (.results)
      <$> liftIO (wait =<< D1.batch d1 (insTagsQ <> tagIdsQ))

  updQ <- articleUpdateBodyQ art upd
  delTagQ <- deleteAllTagsOnArticleQ art
  insTagQ <- tagArticleQ tagIds art
  resl <-
    liftIO $
      wait
        =<< D1.batch d1 (updQ `V.cons` (delTagQ `V.cons` insTagQ))
  unless (all (.success) resl) $
    serverError err500 {errBody = "Failed to update article"}
  pure NoContent

deleteArticle :: AuthResult User -> T.Text -> Handler HumblrEnv NoContent
deleteArticle user slug = protectIfConfigured user do
  met <- liftIO . (wait <=< D1.first) =<< lookupSlugQ slug
  let martId = either (const Nothing) (Just . (.id)) . D1.parseD1RowView @ArticleRow =<< met
  case martId of
    Nothing -> serverError err404 {errBody = "Article not found"}
    Just aid -> do
      delTags <- deleteAllTagsOnArticleQ aid
      delArt <- deleteArticleQ aid
      d1 <- getBinding "D1"
      NoContent <$ liftIO (wait =<< D1.batch d1 (V.fromList [delTags, delArt]))

postArticle :: AuthResult User -> ArticleSeed -> App NoContent
postArticle user art = protectIfConfigured user do
  tryAny (createArticle art) >>= \case
    Left e ->
      serverError err409 {errBody = "Failed to create article: " <> fromString (displayException e)}
    Right () -> pure NoContent

getArticle :: T.Text -> App Article
getArticle slug = do
  maybe
    ( serverError
        err404 {errBody = "Article Not Found: " <> TE.encodeUtf8 slug}
    )
    pure
    =<< lookupSlug slug

listTagArticles :: T.Text -> Maybe Word -> App [Article]
listTagArticles tag mpage = do
  getArticlesWithTag tag $ fromIntegral <$> mpage

listArticles :: Maybe Word -> App [Article]
listArticles mpage = getRecentArticles $ fromIntegral <$> mpage

newtype TagId = TagId {tagId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

newtype ArticleId = ArticleId {articleId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

data TagRow = TagRow {id :: !TagId, name :: !T.Text}
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

getRecentArticles ::
  Maybe Word32 ->
  App [Article]
getRecentArticles page = do
  rows <-
    liftIO . (wait <=< D1.all)
      =<< recentArticlesQ (maybe 0 (* 10) page)
  unless rows.success $
    throwString "Failed to fetch recent articles"
  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM fromArticleRow articles

lookupSlug :: T.Text -> App (Maybe Article)
lookupSlug slug = do
  mrow <- liftIO . (wait <=< D1.first) =<< lookupSlugQ slug
  forM mrow $ \row -> do
    case D1.parseD1RowView row of
      Right r -> fromArticleRow r
      Left err -> throwString err

createArticle :: ArticleSeed -> App ()
createArticle ArticleSeed {tags = ts0, ..} = do
  let tags = V.fromList ts0
  tagQs <- tryInsertTagQ tags
  newArt <- insertArticleQ ArticleSeed {tags = ts0, ..}
  tagIdsQ <- lookupTagsQ tags
  artIdQ <-
    prepareBind "SELECT id FROM articles WHERE slug = ?" $
      V.singleton $
        D1.toD1ValueView slug
  d1 <- getBinding "D1"
  vs <-
    liftIO $
      wait
        =<< D1.batch d1 (newArt `V.cons` tagQs <> tagIdsQ <> V.singleton artIdQ)
  unless (V.all (.success) vs) $
    serverError $
      err409 {errBody = "Failed to insert article: " <> TE.encodeUtf8 slug}
  let (_, rest) = V.splitAt (length tags) $ V.drop 1 vs
      (rawTagIds, V.head -> rawArtId) = V.splitAt (length tags) rest
      tagIds = V.mapMaybe (either (const Nothing) (Just . (.id)) . D1.parseD1RowView @TagRow) $ V.concatMap (.results) rawTagIds
      artId =
        either (error "Failed to parse article") (.id) $
          D1.parseD1RowView @ArticleIdRow $
            V.head rawArtId.results
  tagArtQs <- tagArticleQ tagIds artId
  unless (null tagIds) $
    liftIO $
      void $
        wait =<< D1.batch d1 tagArtQs

newtype ArticleIdRow = ArticleIdRow {id :: ArticleId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row)

getArticlesWithTag :: T.Text -> Maybe Word32 -> App [Article]
getArticlesWithTag tag mpage = do
  rows <-
    liftIO . (wait <=< D1.all)
      =<< articleWithTagsQ tag (fromMaybe 0 mpage)
  unless rows.success $
    throwString "Failed to fetch articles with tag"

  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM fromArticleRow articles

newtype TagName = TagName {name :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row)

getTagName :: TagName -> T.Text
getTagName = coerce

prepareBind :: String -> V.Vector D1.D1ValueView -> App D1.Statement
prepareBind q args = do
  d1 <- getBinding "D1"
  liftIO $ flip D1.bind args =<< D1.prepare d1 q

prepareBinds :: (Traversable t) => String -> t (V.Vector D1.D1ValueView) -> App (t D1.Statement)
prepareBinds q args = do
  d1 <- getBinding "D1"
  liftIO $ D1.prepare d1 q >>= forM args . D1.bind

recentArticlesQ :: Word32 -> App D1.Statement
recentArticlesQ =
  prepareBind "SELECT * FROM articles ORDER BY createdAt DESC LIMIT 10 OFFSET ?"
    . V.singleton
    . D1.toD1ValueView
    . (* 10)

lookupSlugQ :: T.Text -> App D1.Statement
lookupSlugQ =
  prepareBind "SELECT * FROM articles WHERE slug = ?"
    . V.singleton
    . D1.toD1ValueView

articleUpdateBodyQ :: ArticleId -> ArticleUpdate -> App D1.Statement
articleUpdateBodyQ aid ArticleUpdate {..} = do
  now <- liftIO getCurrentTime
  prepareBind "UPDATE articles SET body = ?1, lastUpdate = ?2 WHERE id = ?3" $
    V.fromList
      [ D1.toD1ValueView body
      , D1.toD1ValueView now
      , D1.toD1ValueView aid
      ]

tryInsertTagQ :: V.Vector T.Text -> App (V.Vector D1.Statement)
tryInsertTagQ = do
  prepareBinds "INSERT OR IGNORE INTO tags (name) VALUES (?)"
    . fmap (V.singleton . D1.toD1ValueView)

lookupTagsQ :: V.Vector T.Text -> App (V.Vector D1.Statement)
lookupTagsQ =
  prepareBinds "SELECT * FROM tags WHERE name = ?"
    . fmap (V.singleton . D1.toD1ValueView)

articleTagsQ :: ArticleId -> App D1.Statement
articleTagsQ =
  prepareBind "SELECT tag.name FROM tags tag INNER JOIN articleTags assoc ON tag.id = assoc.tag WHERE assoc.article = ?"
    . V.singleton
    . D1.toD1ValueView

insertArticleQ :: ArticleSeed -> App D1.Statement
insertArticleQ ArticleSeed {..} = do
  now <- liftIO getCurrentTime
  prepareBind "INSERT INTO articles (body, createdAt, lastUpdate, slug) VALUES (?1, ?2, ?3, ?4)" $
    V.fromList
      [ D1.toD1ValueView body
      , D1.toD1ValueView now
      , D1.toD1ValueView now
      , D1.toD1ValueView slug
      ]

tagArticleQ :: V.Vector TagId -> ArticleId -> App (V.Vector D1.Statement)
tagArticleQ tids aid = do
  prepareBinds "INSERT INTO articleTags (article, tag) VALUES (?1, ?2) ON CONFLICT DO NOTHING" $
    V.map (\tid -> V.fromList [D1.toD1ValueView aid, D1.toD1ValueView tid]) tids

articleWithTagsQ :: T.Text -> Word32 -> App D1.Statement
articleWithTagsQ name page = do
  prepareBind "SELECT a.* FROM articles a INNER JOIN articleTags at ON a.id = at.article INNER JOIN tags t ON at.tag = t.id WHERE t.name = ?1 ORDER BY a.createdAt DESC LIMIT 10 OFFSET ?2" $
    V.fromList [D1.toD1ValueView name, D1.toD1ValueView $ page * 10]

deleteAllTagsOnArticleQ :: ArticleId -> App D1.Statement
deleteAllTagsOnArticleQ art = do
  prepareBind "DELETE FROM articleTags WHERE article = ?" $
    V.singleton $
      D1.toD1ValueView art

deleteArticleQ :: ArticleId -> App D1.Statement
deleteArticleQ aid = do
  prepareBind "DELETE FROM articles WHERE id = ?" $
    V.singleton $
      D1.toD1ValueView aid

fromArticleRow ::
  ArticleRow ->
  App Article
fromArticleRow arow = do
  rows <- liftIO . (wait <=< D1.all) =<< articleTagsQ arow.id
  unless rows.success $
    throwString "Failed to fetch tags for article"
  let (fails, tags) = partitionEithers $ map (fmap getTagName . D1.parseD1RowView) $ V.toList rows.results
  unless (null fails) $ throwString $ "Failed to parse tag row: " <> show fails
  pure
    Article
      { body = arow.body
      , slug = arow.slug
      , updatedAt = arow.lastUpdate
      , createdAt = arow.createdAt
      , tags
      }
