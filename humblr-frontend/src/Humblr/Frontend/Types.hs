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
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Frontend.Types (
  Model (..),
  initialModel,
  Mode (..),
  TopPage (..),
  ErrorPage (..),
  EditedArticle (..),
  EditViewState (..),
  Action (..),
  ArticleSeed (..),
  ArticleEdition (..),
  AsRoute,
  callApi,
  api,
  module Humblr.Types,
  adminAPI,
) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Humblr.Types
import Miso
import Miso.String (MisoString)
import Servant.API
import Servant.Auth.Client (Token (..))
import Servant.Client.FetchAPI
import Servant.Client.Generic (genericClient)

initialModel :: Model
initialModel = Model {mode = Idle, errorMessage = Nothing}

data PageOptions = PageOptions
  { title :: !T.Text
  , siteName :: !T.Text
  , topPage :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)

data ArticleEdition = ArticleEdition {body :: !MisoString, tags :: ![T.Text]}
  deriving (Show, Eq, Ord, Generic)

data Mode
  = TopPage !TopPage
  | ArticlePage !Article
  | EditingArticle !EditedArticle
  | CreatingArticle !T.Text !ArticleEdition
  | TagArticles !T.Text !Word ![Article]
  | ErrorPage !ErrorPage
  | Idle
  deriving (Show, Generic, Eq)

data EditedArticle = EditedArticle
  { original :: !Article
  , edition :: !ArticleEdition
  , viewState :: !EditViewState
  }
  deriving (Show, Eq, Generic)

data EditViewState = Edit | Preview
  deriving (Show, Eq, Generic)

data TopPage = MkTopPage {page :: !Word, articles :: ![Article]}
  deriving (Show, Generic, Eq)

data ErrorPage = MkErrorPage
  { title :: !MisoString
  , message :: !MisoString
  }
  deriving (Show, Generic, Eq)

data Model = Model {mode :: !Mode, errorMessage :: !(Maybe T.Text)}
  deriving (Show, Generic, Eq)

{- Note [Naming convention]

- `Open*` means "Start opening page * by retrieving data from the server."
- `Show*` means "Show page * with the given data."
- `NewArticle` never fails, so there is no `Open` or `Show` variant.
-}
data Action
  = ChangeUrl !URI
  | HandleUrl !URI
  | OpenTopPage !(Maybe Word)
  | ShowTopPage !TopPage
  | OpenArticle !T.Text
  | ShowArticle !Article
  | NewArticle
  | OpenEditArticle !T.Text
  | ShowEditArticle !Article
  | SwitchEditViewState !EditViewState
  | OpenTagArticles !T.Text !(Maybe Word)
  | ShowTagArticles !T.Text !Word ![Article]
  | ReportError !MisoString
  | DissmissError
  | ShowErrorPage !MisoString !MisoString
  deriving (Show, Generic)

api :: RestApi (AsClientT (FetchT JSM))
api = (genericClient @RootAPI).apiRoutes

adminAPI :: AdminAPI (AsClientT (FetchT JSM))
adminAPI = api.adminAPI (CloudflareToken Nothing)

type AsRoute :: Type -> Type
data AsRoute a

instance GenericMode (AsRoute a) where
  type AsRoute a :- xs = RouteT xs a

callApi :: FetchT JSM a -> JSM a
callApi act = do
  uri <-
    getCurrentURI
      <&> #uriPath .~ ""
      <&> #uriQuery .~ ""
      <&> #uriFragment .~ ""
  baseUrl <- parseBaseUrl $ show uri
  runFetch baseUrl act
