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
  openTagArticles,
  HasEditView (..),
  saveAction,
  slugG,
  viewStateT,
  bodyT,
  tagsT,
  newTagT,
) where

import Control.Exception.Safe (Exception (..), tryAny)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import GHC.Base (Proxy#, proxy#)
import Humblr.Frontend.Types
import Language.Javascript.JSaddle (setProp, val)
import Language.Javascript.JSaddle.Object (Object (..))
import Miso
import Miso.String (MisoString, ToMisoString (..))
import Miso.String qualified as MisoString
import Network.HTTP.Types (status404)
import Servant.API
import Servant.Auth.Client (Token (CloudflareToken))
import Servant.Client.FetchAPI

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (ChangeUrl url) m =
  m <# do
    pushURI url
    pure (HandleUrl url)
updateModel (HandleUrl url) m = handleUrl url m
updateModel (OpenTopPage mcur) m =
  m <# do
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
      Right articles ->
        pure $
          ShowTopPage $
            MkTopPage PagedArticles {page = fromMaybe 0 mcur, ..}
updateModel (ShowTopPage topPage) m = noEff m {mode = TopPage topPage}
updateModel (OpenArticle slug) m =
  m <# do
    consoleLog $ "Opening article: " <> toMisoString slug
    withArticleSlug slug (pure . ShowArticle)
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
                      callApi $
                        (api.adminAPI (CloudflareToken Nothing)).putArticle
                          original.slug
                          (toArticleUpdate edition)
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
                      callApi $
                        (api.adminAPI (CloudflareToken Nothing)).postArticle
                          ArticleSeed
                            { tags = F.toList fragment.tags
                            , slug = slug
                            , body = fragment.body
                            }
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
                  ArticleFragment {body = mempty, tags = mempty, newTag = "", composingTag = False}
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
      Right articles ->
        pure $
          ShowTagArticles
            tag
            PagedArticles {page = fromMaybe 0 mcur, ..}
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

withArticleSlug :: T.Text -> (Article -> JSM Action) -> JSM Action
withArticleSlug slug k = do
  eith <- tryAny $ callApi (api.getArticle slug)
  case eith of
    Left (fromException -> Just (FailureResponse _req resp))
      | resp.responseStatusCode == status404 -> do
          consoleLog $ "404!"
          pure $ ShowErrorPage "Not Found" $ "The article " <> toMisoString slug <> " was not found."
      | otherwise -> do
          consoleLog $ "ISE (non-404)"
          pure $
            ShowErrorPage "Internal Server Error" $
              "Failed to retrieve the article "
                <> toMisoString slug
                <> ": "
                <> toMisoString (show resp.responseStatusCode)
    Left err -> do
      consoleLog $ "Non-client error"
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
    -- FIXME: Implement Admin Home Page
    adminHome m = noEff m

openTopPage :: Maybe Word -> Action
openTopPage = openEndpoint . rootApiURIs.frontend.topPage

openArticle :: T.Text -> Action
openArticle = openEndpoint . rootApiURIs.frontend.articlePage

openTagArticles :: T.Text -> Maybe Word -> Action
openTagArticles tag = openEndpoint . rootApiURIs.frontend.tagArticles tag

openEndpoint :: URI -> Action
openEndpoint = ChangeUrl

class HasEditView a where
  viewStateL :: Lens' a EditViewState
  slugL :: Either (Getter a MisoString) (Lens' a MisoString)
  tagsL :: Lens' a (Seq MisoString)
  newTagL :: Lens' a MisoString
  bodyL :: Lens' a MisoString
  currentArticle :: a -> Article
  saveAction# :: Proxy# a -> Action
  cancelAction :: a -> Action

slugG :: (HasEditView a) => Getter a MisoString
{-# INLINE slugG #-}
slugG = case slugL of
  Left g -> g
  Right l -> l

instance HasEditView EditedArticle where
  viewStateL = #viewState
  tagsL = #edition . #tags
  bodyL = #edition . #body
  newTagL = #edition . #newTag
  slugL = Left $ #original . #slug
  currentArticle art =
    Article
      { updatedAt = art.original.updatedAt
      , tags = F.toList art.edition.tags
      , slug = art.original.slug
      , createdAt = art.original.createdAt
      , body = art.edition.body
      }
  saveAction# _ = SaveEditingArticle
  cancelAction = openArticle . (.original.slug)

instance HasEditView NewArticle where
  viewStateL = #viewState
  tagsL = #fragment . #tags
  bodyL = #fragment . #body
  slugL = Right #slug
  newTagL = #fragment . #newTag
  currentArticle art =
    Article
      { updatedAt = art.dummyDate
      , tags = F.toList art.fragment.tags
      , slug = art.slug
      , createdAt = art.dummyDate
      , body = art.fragment.body
      }
  saveAction# _ = CreateNewArticle
  cancelAction _ = openTopPage Nothing

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
