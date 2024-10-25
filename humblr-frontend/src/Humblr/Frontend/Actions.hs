{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
) where

import Control.Exception.Safe (Exception (..), tryAny)
import Control.Lens
import Data.Generics.Labels ()
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Humblr.Frontend.Types
import Miso
import Miso.String (ToMisoString (toMisoString))
import Network.HTTP.Types (status404)
import Servant.API
import Servant.Auth.Client (Token (CloudflareToken))
import Servant.Client.FetchAPI

updateModel :: Action -> Model -> Effect Action Model
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
      Right articles -> pure $ ShowTopPage MkTopPage {page = fromMaybe 0 mcur, ..}
updateModel (ShowTopPage topPage) m = noEff m {mode = TopPage topPage}
updateModel (OpenArticle slug) m =
  m <# withArticleSlug slug (pure . ShowArticle)
updateModel (ShowArticle article) m = noEff m {mode = ArticlePage article}
updateModel (OpenEditArticle slug) m =
  m <# withArticleSlug slug (pure . ShowEditArticle)
updateModel (ShowEditArticle article) m =
  noEff
    m
      { mode =
          EditingArticle
            EditedArticle
              { original = article
              , edition = toArticleSeed article
              , viewState = Edit
              }
      }
updateModel (SwitchEditViewState st) m =
  noEff $ m & #mode . #_EditingArticle . #viewState .~ st
updateModel (SetEditingArticleContent f) m =
  noEff $ m & #mode . #_EditingArticle . #edition . #body .~ f
updateModel (DeleteEditingTag f) m =
  noEff $ m & #mode . #_EditingArticle . #edition . #tags %~ L.delete f
updateModel (AddEditingTag f) m =
  noEff $ m & #mode . #_EditingArticle . #edition . #tags %~ L.nub . (f :)
updateModel SaveEditingArticle m =
  m {mode = Idle}
    `batchEff` [ do
                  eith <-
                    tryAny $
                      callApi $
                        (api.adminAPI (CloudflareToken Nothing)).putArticle
                          original.slug
                          edition
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
updateModel NewArticle m =
  noEff
    m
      { mode =
          CreatingArticle
            ""
            ArticleUpdate {body = mempty, tags = mempty}
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
      Right arts -> pure $ ShowTagArticles tag (fromMaybe 0 mcur) arts
updateModel (ShowTagArticles tag cur arts) m =
  noEff m {mode = TagArticles tag cur arts}
updateModel (ShowErrorNotification msg mstate) m =
  noEff $
    m
      & #errorMessage ?~ msg
      & #mode %~ maybe id const mstate
updateModel DismissError m = noEff m {errorMessage = Nothing}
updateModel (ShowErrorPage title message) m =
  noEff m {mode = ErrorPage MkErrorPage {..}}

toArticleSeed :: Article -> ArticleUpdate
toArticleSeed art = ArticleUpdate {body = toMisoString art.body, tags = art.tags}

withArticleSlug :: T.Text -> (Article -> JSM Action) -> JSM Action
withArticleSlug slug k = do
  eith <- tryAny $ callApi (api.getArticle slug)
  case eith of
    Left (fromException -> Just (FailureResponse _req resp))
      | resp.responseStatusCode == status404 ->
          pure $ ShowErrorPage "Not Found" $ "The article " <> toMisoString slug <> " was not found."
      | otherwise ->
          pure $
            ShowErrorPage "Internal Server Error" $
              "Failed to retrieve the article "
                <> toMisoString slug
                <> ": "
                <> toMisoString (show resp.responseStatusCode)
    Left err ->
      pure $
        ShowErrorPage "Internal Server Error" $
          toMisoString $
            displayException err
    Right article -> k article

handleUrl :: URI -> Model -> Effect Action Model
handleUrl url =
  either (\_ m -> m <# pure (OpenTopPage Nothing)) id $
    route @(ToServantApi FrontendRoutes)
      Proxy
      (toServant routes)
      url
  where
    routes :: FrontendRoutes (AsRoute (Model -> Effect Action Model))
    routes = FrontendRoutes {..}
    topPage mcur m = m <# pure (OpenTopPage mcur)
    articlePage slug m = m <# pure (OpenArticle slug)
    newArticle m = m <# pure NewArticle
    editArticle slug m = m <# pure (OpenEditArticle slug)
    tagArticles tag mcur m = m <# pure (OpenTagArticles tag mcur)

openTopPage :: Maybe Word -> Action
openTopPage = openEndpoint . rootApiURIs.frontend.topPage

openArticle :: T.Text -> Action
openArticle = openEndpoint . rootApiURIs.frontend.articlePage

openTagArticles :: T.Text -> Maybe Word -> Action
openTagArticles tag = openEndpoint . rootApiURIs.frontend.tagArticles tag

openEndpoint :: URI -> Action
openEndpoint = ChangeUrl
