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
  gotoTop,
) where

import Control.Exception.Safe (Exception (..), tryAny)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Humblr.Frontend.Types
import Miso
import Miso.String (ToMisoString (toMisoString))
import Network.HTTP.Types (status404)
import Servant.API
import Servant.Auth.Client ()
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
      Left err -> pure $ ReportError $ toMisoString $ displayException err
      Right arts -> pure $ ShowTopPage (fromMaybe 0 mcur) arts
updateModel (ShowTopPage cur arts) m = noEff m {mode = TopPage cur arts}
updateModel (OpenArticle slug) m =
  m <# withArticleSlug slug (pure . ShowArticle)
updateModel (ShowArticle article) m = noEff m {mode = ArticlePage article}
updateModel (OpenEditArticle slug) m =
  m <# withArticleSlug slug (pure . ShowEditArticle)
updateModel (ShowEditArticle article) m =
  noEff m {mode = EditingArticle article (toArticleSeed article)}
updateModel NewArticle m =
  noEff
    m
      { mode =
          CreatingArticle
            ""
            ArticleSeed {body = mempty, tags = mempty}
      }
updateModel (OpenTagArticles tag mcur) m =
  m <# do
    eith <- tryAny $ callApi (api.listTagArticles tag mcur)
    case eith of
      Left err -> pure $ ReportError $ toMisoString $ displayException err
      Right arts -> pure $ ShowTagArticles tag (fromMaybe 0 mcur) arts
updateModel (ShowTagArticles tag cur arts) m =
  noEff m {mode = TagArticles tag cur arts}
updateModel (ReportError msg) m = noEff m {errorMessage = Just msg}
updateModel DissmissError m = noEff m {errorMessage = Nothing}
updateModel (ShowErrorPage title msg) m =
  noEff m {mode = ErrorPage title msg}

toArticleSeed :: Article -> ArticleSeed
toArticleSeed art = ArticleSeed {body = toMisoString art.body, tags = art.tags}

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

gotoTop :: Maybe Word -> Action
gotoTop = openEndpoint . rootApiURIs.frontend.topPage

openEndpoint :: URI -> Action
openEndpoint = ChangeUrl
