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

module Humblr.Worker.Database (
  handlers,
  JSObject (..),
  DatabaseService,
  DatabaseServiceClass,
) where

import Control.Exception.Safe (Exception (..), throwString, tryAny)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor qualified as Bi
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Word
import Humblr.Types
import Network.Cloudflare.Worker.Binding hiding (getBinding, getSecret)
import Network.Cloudflare.Worker.Binding.D1 (D1Class, FromD1Row, FromD1Value, ToD1Row, ToD1Value)
import Network.Cloudflare.Worker.Binding.D1 qualified as D1
import Network.Cloudflare.Worker.Binding.Service

type App = ServiceM DbEnv '[]

data DatabaseServiceFuns = DatabaseServiceFuns
  { putArticle :: T.Text -> ArticleUpdate -> App ()
  , deleteArticle :: T.Text -> App ()
  , postArticle :: ArticleSeed -> App ()
  , getArticle :: T.Text -> App Article
  , listTagArticles :: T.Text -> Maybe Word -> App [Article]
  , listArticles :: Maybe Word -> App [Article]
  }
  deriving (Generic)
  deriving anyclass (ToService DbEnv)

type DbFuns = Signature DbEnv DatabaseServiceFuns

type DatabaseService = Service DbFuns

type DatabaseServiceClass = ServiceClass DbFuns

type DbEnv =
  BindingsClass
    '[]
    '[]
    '[ '("D1", D1Class)]

handlers :: IO (Service DbFuns)
handlers =
  toService @DbEnv
    DatabaseServiceFuns
      { putArticle
      , deleteArticle
      , postArticle
      , getArticle
      , listTagArticles
      , listArticles
      }

putArticle :: T.Text -> ArticleUpdate -> App ()
putArticle slug upd = do
  tryInsertTagQ <- mkTryInsertTagQ
  lookupTagQ <- mkLookupTagNameQ
  tagArticleQ <- mkTagArticleQ
  lookSlugQ <- mkLookupSlugQ
  art <-
    either (throwString . ("Schema Error (article): " <>)) (pure . (.id))
      . D1.parseD1RowView @ArticleRow
      =<< maybe (throwString $ ("Article not found: " <>) $ T.unpack slug) pure
      =<< liftIO . (await' <=< D1.first)
      =<< bind lookSlugQ slug
  -- FIXME: make these two together with @art@ into a single batch.
  insTagsQ <- mapM (bind tryInsertTagQ) upd.tags
  tagIdsQ <- mapM (bind lookupTagQ) upd.tags
  d1 <- getBinding "D1"
  tagIds <-
    V.toList
      . V.mapMaybe (either (const Nothing) (Just . (.id)) . D1.parseD1RowView @TagRow)
      . V.concatMap (.results)
      <$> liftIO (await' =<< D1.batch d1 (V.fromList $ insTagsQ ++ tagIdsQ))

  updQ <- mkArticleUpdateBody >>= \q -> bind q art upd
  delTagQ <- mkDeleteAllTagsOnArticle >>= \q -> bind q art
  insTagQ <- mapM (bind tagArticleQ art) tagIds
  resl <-
    liftIO $
      await'
        =<< D1.batch d1 (V.fromList $ updQ : delTagQ : insTagQ)
  unless (all (.success) resl) $
    throwString "Failed to update article"

deleteArticle :: T.Text -> App ()
deleteArticle slug = do
  lookupFromSlug <- mkLookupSlugQ
  met <- liftIO . (await' <=< D1.first) =<< bind lookupFromSlug slug
  let martId = either (const Nothing) (Just . (.id)) . D1.parseD1RowView @ArticleRow =<< met
  case martId of
    Nothing -> throwString "Article not found"
    Just aid -> do
      delQ <- mkDeleteAllTagsOnArticle
      delTags <- bind delQ aid
      delArtQ <- mkDeleteArticle
      delArt <- bind delArtQ aid
      d1 <- getBinding "D1"
      waitUntil . jsPromise =<< liftIO (D1.batch d1 (V.fromList [delTags, delArt]))

postArticle :: ArticleSeed -> App ()
postArticle art = do
  tryAny (createArticle art) >>= \case
    Left e ->
      throwString $ "Failed to create article: " <> displayException e
    Right () -> pure ()

getArticle :: T.Text -> App Article
getArticle slug = do
  maybe
    (throwString $ "Article Not Found: " <> T.unpack slug)
    pure
    =<< lookupSlug slug

listTagArticles :: T.Text -> Maybe Word -> App [Article]
listTagArticles tag mpage =
  getArticlesWithTag tag $ fromIntegral <$> mpage

listArticles :: Maybe Word -> App [Article]
listArticles mpage = getRecentArticles (fromIntegral <$> mpage)

newtype TagId = TagId {tagId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

newtype ArticleId = ArticleId {articleId :: Word32}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromD1Value, ToD1Value)

newtype ImageId = ImageId {articleId :: Word32}
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

data ImageRow = ImageRow
  { id :: !ImageId
  , link :: !T.Text
  , name :: !T.Text
  , ctype :: !ImageType
  , offset :: !Word32
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row, ToD1Row)

type family xs ~~> a where
  '[] ~~> a = a
  (x ': xs) ~~> a = x -> (xs ~~> a)

newtype Preparation params = Preparation (params ~~> App D1.Statement)

bind :: forall params. Preparation params -> params ~~> App D1.Statement
bind = coerce

getRecentArticles ::
  Maybe Word32 ->
  App [Article]
getRecentArticles page = do
  recents <- mkRecentArticlesQ
  rows <-
    liftIO . (await' <=< D1.all)
      =<< bind recents (maybe 0 (* 10) page)
  unless rows.success $
    throwString "Failed to fetch recent articles"
  let (fails, articles) = partitionEithers $ map D1.parseD1RowView $ V.toList rows.results
  unless (null fails) $
    throwString $
      "Failed to parse article row: " <> show fails
  mapM fromArticleRow articles

mkRecentArticlesQ :: App (Preparation '[Word32])
mkRecentArticlesQ = do
  d1 <- getBinding "D1"
  liftIO (D1.prepare d1 "SELECT * FROM articles ORDER BY createdAt DESC LIMIT 10 OFFSET ?") <&> \prep ->
    Preparation \page ->
      liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView $ page * 10)

mkLookupSlugQ :: App (Preparation '[T.Text])
mkLookupSlugQ = do
  d1 <- getBinding "D1"
  liftIO $
    D1.prepare d1 "SELECT * FROM articles WHERE slug = ?" <&> \prep ->
      Preparation \slug ->
        liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView slug)

mkArticleUpdateBody :: App (Preparation '[ArticleId, ArticleUpdate])
mkArticleUpdateBody = do
  prepare "UPDATE articles SET body = ?1, lastUpdate = ?2 WHERE id = ?3" <&> \prep ->
    Preparation \aid ArticleUpdate {..} -> do
      now <- liftIO getCurrentTime
      liftIO $
        D1.bind prep $
          V.fromList
            [ D1.toD1ValueView body
            , D1.toD1ValueView now
            , D1.toD1ValueView aid
            ]

lookupSlug :: T.Text -> App (Maybe Article)
lookupSlug slug = do
  lookupQ <- mkLookupSlugQ
  mrow <- liftIO . (await' <=< D1.first) =<< bind lookupQ slug
  forM mrow $ \row -> do
    case D1.parseD1RowView row of
      Right r -> fromArticleRow r
      Left err -> throwString err

prepare :: String -> App D1.PreparedStatement
prepare q = do
  d1 <- getBinding "D1"
  liftIO $ D1.prepare d1 q

mkTryInsertTagQ :: App (Preparation '[T.Text])
mkTryInsertTagQ =
  prepare "INSERT OR IGNORE INTO tags (name) VALUES (?)"
    <&> \prep -> Preparation \name ->
      liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkLookupTagNameQ :: App (Preparation '[T.Text])
mkLookupTagNameQ =
  prepare "SELECT * FROM tags WHERE name = ?" <&> \prep ->
    Preparation \name ->
      liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView name)

mkArticleTagsQ :: App (Preparation '[ArticleId])
mkArticleTagsQ =
  prepare "SELECT tag.name FROM tags tag INNER JOIN articleTags assoc ON tag.id = assoc.tag WHERE assoc.article = ?" <&> \prep ->
    Preparation \aid ->
      liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView aid)

mkArticleImagesQ :: App (Preparation '[ArticleId])
mkArticleImagesQ =
  prepare "SELECT img.*, assoc.offset FROM images img INNER JOIN articleImages assoc on img.id = assoc.image_ WHERE assoc.article = ? ORDER BY assoc.offset ASC" <&> \prep ->
    Preparation \aid ->
      liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView aid)

createArticle :: ArticleSeed -> App ()
createArticle ArticleSeed {..} = do
  tryInsertQ <- mkTryInsertTagQ
  insertArtQ <- mkInsertArticleQ
  lookupTagQ <- mkLookupTagNameQ
  tagArtQ <- mkTagArticleQ
  tagQs <- mapM (bind tryInsertQ) tags
  newArt <- bind insertArtQ ArticleSeed {..}
  tagIdsQ <- mapM (bind lookupTagQ) tags
  artIdQ <-
    prepare "SELECT id FROM articles WHERE slug = ?" >>= \q ->
      liftIO (D1.bind q $ V.singleton $ D1.toD1ValueView slug)
  d1 <- getBinding "D1"
  vs <-
    liftIO $
      await'
        =<< D1.batch d1 (V.fromList $ newArt : tagQs ++ tagIdsQ ++ [artIdQ])
  unless (V.all (.success) vs) $
    throwString $
      "Failed to insert article: " <> T.unpack slug
  let (_, rest) = V.splitAt (length tags) $ V.drop 1 vs
      (rawTagIds, V.head -> rawArtId) = V.splitAt (length tags) rest
      tagIds = V.mapMaybe (either (const Nothing) (Just . (.id)) . D1.parseD1RowView @TagRow) $ V.concatMap (.results) rawTagIds
      artId =
        either (error "Failed to parse article") (.id) $
          D1.parseD1RowView @ArticleIdRow $
            V.head rawArtId.results
  unless (null tagIds) $
    void $
      liftIO . D1.batch d1
        =<< mapM (bind tagArtQ artId) tagIds

newtype ArticleIdRow = ArticleIdRow {id :: ArticleId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromD1Row)

mkInsertArticleQ :: App (Preparation '[ArticleSeed])
mkInsertArticleQ =
  prepare "INSERT INTO articles (body, createdAt, lastUpdate, slug) VALUES (?1, ?2, ?3, ?4)" <&> \prep ->
    Preparation \ArticleSeed {..} -> do
      now <- liftIO getCurrentTime
      liftIO $
        D1.bind prep $
          V.fromList
            [ D1.toD1ValueView body
            , D1.toD1ValueView now
            , D1.toD1ValueView now
            , D1.toD1ValueView slug
            ]

mkTagArticleQ :: App (Preparation '[ArticleId, TagId])
mkTagArticleQ =
  prepare "INSERT INTO articleTags (article, tag) VALUES (?1, ?2) ON CONFLICT DO NOTHING" <&> \prep ->
    Preparation \aid tid ->
      liftIO $
        D1.bind prep $
          V.fromList
            [ D1.toD1ValueView aid
            , D1.toD1ValueView tid
            ]

mkArticleWithTagsQ :: App (Preparation '[T.Text, Word32])
mkArticleWithTagsQ =
  prepare "SELECT a.* FROM articles a INNER JOIN articleTags at ON a.id = at.article INNER JOIN tags t ON at.tag = t.id WHERE t.name = ?1 ORDER BY a.createdAt DESC LIMIT 10 OFFSET ?2" <&> \prep ->
    Preparation \name page ->
      liftIO $ D1.bind prep (V.fromList [D1.toD1ValueView name, D1.toD1ValueView $ page * 10])

mkDeleteAllTagsOnArticle :: App (Preparation '[ArticleId])
mkDeleteAllTagsOnArticle = do
  d1 <- getBinding "D1"
  liftIO $
    D1.prepare d1 "DELETE FROM articleTags WHERE article = ?" <&> \prep ->
      Preparation \art ->
        liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView art)

mkDeleteArticle :: App (Preparation '[ArticleId])
mkDeleteArticle = do
  d1 <- getBinding "D1"
  liftIO $
    D1.prepare d1 "DELETE FROM articles WHERE id = ?" <&> \prep ->
      Preparation \aid ->
        liftIO $ D1.bind prep (V.singleton $ D1.toD1ValueView aid)

getArticlesWithTag :: T.Text -> Maybe Word32 -> App [Article]
getArticlesWithTag tag mpage = do
  artsWithTagQ <- mkArticleWithTagsQ
  rows <-
    liftIO . (await' <=< D1.all)
      =<< bind artsWithTagQ tag (fromMaybe 0 mpage)
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

fromArticleRow ::
  ArticleRow ->
  App Article
fromArticleRow arow = do
  artTagsQ <- mkArticleTagsQ
  tagRows <- liftIO . (await' <=< D1.all) =<< bind artTagsQ arow.id
  unless tagRows.success $
    throwString "Failed to fetch tags for article"
  let (fails, tags) = partitionEithers $ map (fmap getTagName . D1.parseD1RowView) $ V.toList tagRows.results
  unless (null fails) $ throwString $ "Failed to parse tag row: " <> show fails

  -- Fetch associated images.
  artImgsQ <- mkArticleImagesQ
  imgRows <- liftIO . (await' <=< D1.all) =<< bind artImgsQ arow.id
  unless imgRows.success $
    throwString "Failed to fetch images for article"
  let (failImgs, attachments) =
        Bi.second (map fromImageRow) $
          partitionEithers $
            map (D1.parseD1RowView @ImageRow) $
              V.toList imgRows.results
  unless (null failImgs) $ throwString $ "Failed to parse image row: " <> show (failImgs, V.toList imgRows.results)
  pure
    Article
      { body = arow.body
      , slug = arow.slug
      , updatedAt = arow.lastUpdate
      , createdAt = arow.createdAt
      , tags
      , attachments
      }

fromImageRow :: ImageRow -> Attachment
fromImageRow img = Attachment {ctype = img.ctype, url = img.link, name = img.name}
