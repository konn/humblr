{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Frontend.Actions (
  updateModel,
  openEndpoint,
  openTopPage,
  openArticle,
  openAdminPage,
  openEditArticle,
  openTagArticles,
  openNewArticle,
  HasEditView (..),
  saveAction,
  slugG,
  viewStateT,
  bodyT,
  tagsT,
  newTagT,
  SlugMode (..),
  slugL,
  HasArticles (..),
  articleAction,
  articlesT,
  BlobURLs (..),
) where

import Control.Arrow ((&&&))
import Control.Exception.Safe (Exception (..), tryAny)
import Control.Lens hiding ((#))
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable qualified as F
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Map.Ordered.Strict qualified as OM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Vector qualified as V
import GHC.Base (Proxy#, proxy#)
import GHC.Generics (Generic)
import Humblr.CMark (getSummary)
import Humblr.CMark qualified as CM
import Humblr.Frontend.Types
import Language.Javascript.JSaddle (FromJSVal (..), getProp, isUndefined, jsf, setProp, toJSVal_aeson, val, (#))
import Language.Javascript.JSaddle qualified as JSM
import Language.Javascript.JSaddle.Evaluate (eval)
import Language.Javascript.JSaddle.Object (Object (..))
import Miso
import Miso.String (MisoString, ToMisoString (..))
import Miso.String qualified as MisoString
import Network.HTTP.Types (status404)
import Servant.API (
  NoContent (NoContent),
  ToServantApi,
  toServant,
  toUrlPiece,
 )
import Servant.Auth.Client (Token (CloudflareToken))
import Servant.Client.FetchAPI

default (T.Text)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (ChangeUrl url) m =
  m <# do
    pushURI url
    pure (HandleUrl url)
updateModel (HandleUrl url) m = handleUrl url m
updateModel (OpenAdminPage mcur) m =
  m <# do
    withArticles mcur $ pure . ShowAdminPage . MkAdminPage
updateModel (ShowAdminPage adminPage) m = noEff m {mode = AdminPage adminPage}
updateModel (OpenTopPage mcur) m =
  m <# do
    withArticles mcur $ pure . ShowTopPage . MkTopPage
updateModel (ShowTopPage topPage) m = noEff m {mode = TopPage topPage}
updateModel (OpenArticle slug) m =
  m <# withArticleSlug slug (pure . ShowArticle)
updateModel (ShowArticle article) m = noEff m {mode = ArticlePage article}
updateModel (SwitchEditViewState st) m =
  noEff $ m & #mode . viewStateT .~ st
updateModel (OpenEditArticle slug) m =
  m <# withArticleSlug slug (pure . ShowEditArticle)
updateModel (ShowEditArticle article) m =
  noEff
    m
      { mode =
          EditingArticle
            EditedArticle
              { original = article
              , edition = toArticleEdition article
              , viewState = Edit
              }
      }
updateModel (SetEditingArticleContent f) m =
  noEff $ m & #mode . bodyT .~ f
updateModel (DeleteEditingTag f) m =
  noEff $ m & #mode . tagsT %~ Seq.filter (/= f)
updateModel AddEditingTag m
  | MisoString.null (m ^. #mode . newTagT) = noEff m
  | otherwise =
      let m' =
            m
              & #mode . tagsT |>~ m ^. #mode . newTagT
              & #mode . newTagT .~ ""
       in m' <# do
            field <- getElementById newTagInputId
            emp <- val ("" :: T.Text)
            setProp "value" emp $ Object field
            pure NoOp
updateModel SaveEditingArticle m =
  m {mode = Idle}
    `batchEff` [ do
                  eith <-
                    tryAny $
                      callApi
                        . (api.adminAPI (CloudflareToken Nothing)).putArticle
                          original.slug
                        =<< toArticleUpdate original.slug edition
                  case eith of
                    Left err ->
                      pure $
                        ShowErrorNotification
                          MkErrorMessage
                            { title = "Could not save article " <> original.slug
                            , message = toMisoString $ displayException err
                            }
                          (Just m.mode)
                    Right NoContent -> pure $ openArticle original.slug
               | EditedArticle {..} <- m ^.. #mode . #_EditingArticle
               ]
updateModel CreateNewArticle m =
  m {mode = Idle}
    `batchEff` [ do
                  eith <-
                    tryAny $
                      callApi
                        . (api.adminAPI (CloudflareToken Nothing)).postArticle
                        =<< toArticleSeed slug fragment
                  case eith of
                    Left err ->
                      pure $
                        ShowErrorNotification
                          MkErrorMessage
                            { title = "Could not save article " <> slug
                            , message = toMisoString $ displayException err
                            }
                          (Just m.mode)
                    Right NoContent -> pure $ openArticle slug
               | MkNewArticle {..} <- m ^.. #mode . #_CreatingArticle
               ]
updateModel (SetNewTagName f) m =
  noEff $ m & #mode . newTagT .~ f
updateModel OpenNewArticle m =
  m <# do ShowNewArticle <$> liftIO getCurrentTime
updateModel (ShowNewArticle stamp) m =
  noEff
    m
      { mode =
          CreatingArticle
            MkNewArticle
              { slug = T.pack $ formatTime defaultTimeLocale "%Y%m%d-%H-%M" stamp
              , fragment =
                  ArticleFragment {body = mempty, tags = mempty, newTag = "", blobURLs = mempty}
              , viewState = Edit
              , dummyDate = stamp
              }
      }
updateModel (OpenTagArticles tag mcur) m =
  m <# do
    eith <- tryAny $ callApi (api.listTagArticles tag mcur)
    case eith of
      Left err ->
        pure $
          ShowErrorNotification
            MkErrorMessage
              { title = "Tag Not Found"
              , message = "Tag retrieve failed: " <> toMisoString (displayException err)
              }
            Nothing
      Right articles -> pure $ ShowTagArticles tag articles
updateModel (ShowTagArticles tag articles) m =
  noEff m {mode = TagArticles MkTagArticles {..}}
updateModel (ShowErrorNotification msg mstate) m =
  noEff $
    m
      & #errorMessage ?~ msg
      & #mode %~ maybe id const mstate
updateModel DismissError m = noEff m {errorMessage = Nothing}
updateModel (ShowErrorPage title message) m =
  noEff m {mode = ErrorPage MkErrorPage {..}}
updateModel (SetEditedSlug slg) m =
  noEff $ m & #mode . #_CreatingArticle . #slug .~ slg
updateModel (SetFieldValue fid v) m =
  m <# do
    field <- getElementById fid
    emp <- val v
    setProp "value" emp $ Object field
    pure NoOp
updateModel (ShareArticle art) m =
  m <# do
    share <- eval ("navigator.share" :: String)
    absent <- ghcjsPure $ isUndefined share
    rootUri <-
      getCurrentURI
        <&> #uriPath .~ ""
        <&> #uriQuery .~ ""
        <&> #uriFragment .~ ""
    let url =
          rootUri {uriPath = "/" <> T.unpack (toUrlPiece $ rootApiLinks.frontend.articlePage art.slug)}
        title = CM.nodeToPlainText $ (fromMaybe <$> id <*> getSummary) $ CM.commonmarkToNode [] art.body
        shareDesc = ShareInfo {text = title, ..}
    if absent
      then pure $ ShowModal $ Share shareDesc
      else do
        shared <- toJSVal_aeson shareDesc
        NoOp <$ (eval ("navigator" :: String) # ("share" :: String) $ shared)
updateModel (ShowModal modal) m = noEff m {modal = Just modal}
updateModel DismissModal m = noEff m {modal = Nothing}
updateModel (CopyValueById eid) m =
  m <# do
    eith <- tryAny $ getElementById eid
    forM_ eith $ \field -> do
      clip <- JSM.eval ("navigator.clipboard" :: String)
      msg <- getProp "value" (Object field)
      void $ JSM.liftJSM $ clip JSM.# ("writeText" :: String) $ msg
    pure NoOp
updateModel (DeleteArticle slug) m =
  m <# do
    eith <- tryAny $ callApi (adminAPI.deleteArticle slug)
    case eith of
      Right NoContent -> pure $ openAdminPage Nothing
      Left err -> pure $ ShowErrorNotification (MkErrorMessage "Could not delete article" $ toMisoString $ displayException err) Nothing
updateModel (FileChanged (ElementId eid)) m =
  m <# do
    eith <- tryAny $ getElementById eid
    resl <- forM eith \file -> do
      files <- getProp "files" (Object file)
      numFiles <- fmap (fromMaybe 0) . fromJSVal =<< getProp "length" (Object files)
      if numFiles <= 0
        then pure NoOp
        else do
          urls <- V.generateM numFiles \i -> do
            f <- files ^. jsf ("item" :: String) (val i)
            mctype <- fmap (parseImageCType =<<) . fromJSVal =<< (getProp "type" $ Object f)
            mname <- fromJSVal =<< (getProp "name" $ Object f)
            murl <-
              fmap (fmap TempImg) . fromJSVal
                =<< eval ("URL" :: String) ^. jsf ("createObjectURL" :: String) f
            forM ((,,) <$> mctype <*> murl <*> mname) \(ctype, url, name) ->
              pure EditedAttachment {..}

          empStr <- val ("" :: T.Text)
          setProp "value" empStr (Object file)
          pure $ AddBlobURLs $ BlobURLs $ OM.fromList $ map ((.name) &&& id) $ catMaybes $ V.toList urls
    either (const $ pure NoOp) pure resl
updateModel (AddBlobURLs urls) m =
  noEff $ m & #mode . blobURLsT <>~ urls
updateModel (RemoveBlobURL url) m =
  noEff $ m & #mode . blobURLsT . #urls %~ OM.filter (const $ (/= url) . (.url))

withArticles :: Maybe Word -> (Paged Article -> JSM Action) -> JSM Action
withArticles mcur k = do
  eith <- tryAny $ callApi (api.listArticles mcur)
  case eith of
    Left err ->
      pure $
        ShowErrorNotification
          MkErrorMessage
            { title = "Could not Retrieve Articles!"
            , message = toMisoString $ displayException err
            }
          Nothing
    Right articles -> k articles

withArticleSlug :: T.Text -> (Article -> JSM Action) -> JSM Action
withArticleSlug slug k = do
  eith <- tryAny $ callApi (api.getArticle slug)
  case eith of
    Left (fromException -> Just (FailureResponse _req resp))
      | resp.responseStatusCode == status404 ->
          pure $ ShowErrorPage "Not Found" $ "The article " <> toMisoString slug <> " was not found."
      | otherwise -> do
          pure $
            ShowErrorPage "Internal Server Error" $
              "Failed to retrieve the article "
                <> toMisoString slug
                <> ": "
                <> toMisoString (show resp.responseStatusCode)
    Left err -> do
      pure $
        ShowErrorPage "Internal Server Error" $
          toMisoString $
            displayException err
    Right article -> k article

handleUrl :: URI -> Model -> Effect Action Model
handleUrl url =
  either (\_ m -> m <# pure (openTopPage Nothing)) id $
    route @(ToServantApi FrontendRoutes)
      Proxy
      (toServant routes)
      url
  where
    routes :: FrontendRoutes (AsRoute (Model -> Effect Action Model))
    routes = FrontendRoutes {..}
    topPage mcur m = m <# pure (OpenTopPage mcur)
    articlePage slug m = m <# pure (OpenArticle slug)
    newArticle m = m <# pure OpenNewArticle
    editArticle slug m = m <# pure (OpenEditArticle slug)
    tagArticles tag mcur m = m <# pure (OpenTagArticles tag mcur)
    adminHome mcur m = m <# pure (OpenAdminPage mcur)

openTopPage :: Maybe Word -> Action
openTopPage = openEndpoint . rootApiURIs.frontend.topPage

openArticle :: T.Text -> Action
openArticle = openEndpoint . rootApiURIs.frontend.articlePage

openEditArticle :: T.Text -> Action
openEditArticle = openEndpoint . rootApiURIs.frontend.editArticle

openAdminPage :: Maybe Word -> Action
openAdminPage = openEndpoint . rootApiURIs.frontend.adminHome

openTagArticles :: T.Text -> Maybe Word -> Action
openTagArticles tag = openEndpoint . rootApiURIs.frontend.tagArticles tag

openNewArticle :: Action
openNewArticle = openEndpoint rootApiURIs.frontend.newArticle

openEndpoint :: URI -> Action
openEndpoint = ChangeUrl

data SlugMode a
  = FixedSlug (ReifiedGetter a MisoString)
  | DynamicSlug (ReifiedLens' a MisoString)
  deriving (Generic)

class HasEditView a where
  viewStateL :: Lens' a EditViewState
  slugMode :: SlugMode a
  tagsL :: Lens' a (Seq MisoString)
  newTagL :: Lens' a MisoString
  bodyL :: Lens' a MisoString
  currentArticle :: a -> Article
  saveAction# :: Proxy# a -> Action
  cancelAction :: a -> Action
  blobURLsL :: Lens' a BlobURLs

slugG :: (HasEditView a) => Getter a MisoString
{-# INLINE slugG #-}
slugG = case slugMode of
  FixedSlug g -> runGetter g
  DynamicSlug l -> runLens l

slugL :: (HasEditView a) => Maybe (Lens' a MisoString)
{-# INLINE slugL #-}
slugL = case slugMode of
  FixedSlug {} -> Nothing
  DynamicSlug l -> Just (runLens l)

instance HasEditView EditedArticle where
  viewStateL = #viewState
  tagsL = #edition . #tags
  bodyL = #edition . #body
  newTagL = #edition . #newTag
  slugMode = FixedSlug $ Getter $ #original . #slug
  currentArticle art =
    Article
      { updatedAt = art.original.updatedAt
      , tags = F.toList art.edition.tags
      , slug = art.original.slug
      , createdAt = art.original.createdAt
      , body = art.edition.body
      , attachments = map fromEditedAttachment $ F.toList art.edition.blobURLs.urls
      }
  saveAction# _ = SaveEditingArticle
  cancelAction = openArticle . (.original.slug)
  blobURLsL = #edition . #blobURLs

instance HasEditView NewArticle where
  viewStateL = #viewState
  tagsL = #fragment . #tags
  bodyL = #fragment . #body
  slugMode = DynamicSlug $ Lens #slug
  newTagL = #fragment . #newTag
  currentArticle art =
    Article
      { updatedAt = art.dummyDate
      , tags = F.toList art.fragment.tags
      , slug = art.slug
      , createdAt = art.dummyDate
      , body = art.fragment.body
      , attachments =
          map fromEditedAttachment $
            F.toList art.fragment.blobURLs.urls
      }
  saveAction# _ = CreateNewArticle
  cancelAction _ = openTopPage Nothing
  blobURLsL = #fragment . #blobURLs

saveAction :: forall state -> (HasEditView state) => Action
{-# INLINE saveAction #-}
saveAction state = saveAction# @state proxy#

viewStateT :: Traversal' Mode EditViewState
viewStateT =
  failing
    (#_EditingArticle . viewStateL)
    (#_CreatingArticle . viewStateL)

bodyT :: Traversal' Mode MisoString
bodyT =
  failing
    (#_EditingArticle . bodyL)
    (#_CreatingArticle . bodyL)

tagsT :: Traversal' Mode (Seq MisoString)
tagsT =
  failing
    (#_EditingArticle . tagsL)
    (#_CreatingArticle . tagsL)

newTagT :: Traversal' Mode MisoString
newTagT =
  failing
    (#_EditingArticle . newTagL)
    (#_CreatingArticle . newTagL)

class HasArticles a where
  articlesL :: Lens' a (Paged Article)
  articleAction# :: Proxy# a -> T.Text -> Action
  gotoPageAction :: a -> Word -> Action

articleAction :: forall a -> (HasArticles a) => T.Text -> Action
{-# INLINE articleAction #-}
articleAction a = articleAction# @a proxy#

instance HasArticles TagArticles where
  articlesL = #articles
  articleAction# _ = openArticle
  gotoPageAction a = openTagArticles a.tag . Just

instance HasArticles TopPage where
  articlesL = #articles
  articleAction# _ = openArticle
  gotoPageAction _ = openTopPage . Just

instance HasArticles AdminPage where
  articlesL = #articles
  articleAction# _ = openEditArticle
  gotoPageAction _ = openAdminPage . Just

articlesT :: Traversal' Mode (Paged Article)
articlesT =
  failing
    ( failing
        (#_TopPage . articlesL)
        (#_TagArticles . articlesL)
    )
    (#_AdminPage . articlesL)

blobURLsT :: Traversal' Mode BlobURLs
blobURLsT =
  failing
    (#_EditingArticle . blobURLsL)
    (#_CreatingArticle . blobURLsL)
