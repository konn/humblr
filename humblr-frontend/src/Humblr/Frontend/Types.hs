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
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Frontend.Types (
  Model (..),
  initialModel,
  Mode (..),
  isAdminMode,
  TopPage (..),
  AdminPage (..),
  ErrorPage (..),
  ErrorMessage (..),
  ArticleFragment (..),
  EditViewState (..),
  EditedArticle (..),
  NewArticle (..),
  TagArticles (..),
  PagedArticles (..),
  toArticleEdition,
  toArticleUpdate,
  Action (..),
  ArticleSeed (..),
  AsRoute,
  callApi,
  api,
  module Humblr.Types,
  adminAPI,
  newTagInputId,
) where

import Control.Lens
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Humblr.Types
import Language.Javascript.JSaddle
import Miso
import Miso.String (MisoString)
import Servant.API
import Servant.Auth.Client (Token (..))
import Servant.Client.FetchAPI
import Servant.Client.Generic (genericClient)

initialModel :: Model
initialModel =
  Model
    { mode = Idle
    , errorMessage = Nothing
    }

data ErrorMessage = MkErrorMessage {title, message :: MisoString}
  deriving (Show, Eq, Generic)

data PageOptions = PageOptions
  { title :: !T.Text
  , siteName :: !T.Text
  , topPage :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)

data Mode
  = TopPage !TopPage
  | ArticlePage !Article
  | TagArticles !TagArticles
  | AdminPage !AdminPage
  | EditingArticle !EditedArticle
  | CreatingArticle !NewArticle
  | ErrorPage !ErrorPage
  | Idle
  deriving (Show, Generic, Eq)

isAdminMode :: Mode -> Bool
isAdminMode TopPage {} = False
isAdminMode ArticlePage {} = False
isAdminMode TagArticles {} = False
isAdminMode AdminPage {} = True
isAdminMode EditingArticle {} = True
isAdminMode CreatingArticle {} = True
isAdminMode ErrorPage {} = False
isAdminMode Idle = False

data TagArticles = MkTagArticles
  { tag :: !T.Text
  , articles :: {-# UNPACK #-} !PagedArticles
  }
  deriving (Show, Generic, Eq)

data PagedArticles = PagedArticles
  { page :: !Word
  , articles :: ![Article]
  }
  deriving (Show, Generic, Eq)

data NewArticle = MkNewArticle
  { slug :: !MisoString
  , fragment :: !ArticleFragment
  , viewState :: !EditViewState
  , dummyDate :: !UTCTime
  }
  deriving (Show, Generic, Eq)

data ArticleFragment = ArticleFragment
  { body :: !MisoString
  , tags :: !(Seq MisoString)
  , newTag :: !MisoString
  , composingTag :: !Bool
  }
  deriving (Show, Eq, Generic)

toArticleUpdate :: ArticleFragment -> ArticleUpdate
toArticleUpdate ArticleFragment {..} = ArticleUpdate {tags = F.toList tags, ..}

toArticleEdition :: Article -> ArticleFragment
toArticleEdition Article {..} =
  ArticleFragment
    { newTag = ""
    , composingTag = False
    , tags = Seq.fromList tags
    , ..
    }

data EditedArticle = EditedArticle
  { original :: !Article
  , edition :: !ArticleFragment
  , viewState :: !EditViewState
  }
  deriving (Show, Eq, Generic)

data EditViewState = Edit | Preview
  deriving (Show, Eq, Generic)

newtype TopPage = MkTopPage {articles :: PagedArticles}
  deriving (Show, Generic, Eq)

newtype AdminPage = MkAdminPage {articles :: PagedArticles}
  deriving (Show, Generic, Eq)

data ErrorPage = MkErrorPage
  { title :: !MisoString
  , message :: !MisoString
  }
  deriving (Show, Generic, Eq)

data Model = Model {mode :: !Mode, errorMessage :: !(Maybe ErrorMessage)}
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
  | OpenAdminPage !(Maybe Word)
  | ShowAdminPage !AdminPage
  | OpenNewArticle
  | ShowNewArticle !UTCTime
  | CreateNewArticle
  | OpenEditArticle !T.Text
  | ShowEditArticle !Article
  | SwitchEditViewState !EditViewState
  | SetEditingArticleContent !MisoString
  | DeleteEditingTag !MisoString
  | AddEditingTag
  | SetNewTagName !MisoString
  | SaveEditingArticle
  | OpenTagArticles !T.Text !(Maybe Word)
  | ShowTagArticles !T.Text !PagedArticles
  | SetEditedSlug !MisoString
  | ShowErrorNotification !ErrorMessage !(Maybe Mode)
  | DismissError
  | ShowErrorPage !MisoString !MisoString
  | NoOp
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

newTagInputId :: MisoString
newTagInputId = "new-tag-input"
