{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
  entrypointId,
  Mode (..),
  isAdminMode,
  Modal (..),
  ShareInfo (..),
  BlobURLs (..),
  EditedAttachment (..),
  ImageUrl (..),
  ImageSize (..),
  attachmentUrl,
  fromAttachment,
  fromEditedAttachment,
  ElementId (..),
  TopPage (..),
  AdminPage (..),
  ErrorPage (..),
  ErrorMessage (..),
  ArticleFragment (..),
  EditViewState (..),
  EditedArticle (..),
  NewArticle (..),
  TagArticles (..),
  toArticleEdition,
  toArticleUpdate,
  toArticleSeed,
  resouceUrl,
  Action (..),
  ArticleSeed (..),
  AsRoute,
  callApi,
  api,
  module Humblr.Types,
  adminAPI,
  newTagInputId,
  slugFieldId,
  shareAreaId,
  fileInputId,
) where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OM
import Data.Proxy (Proxy (..))
import Data.Semigroup (First (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins hiding (inject)
import GHC.Wasm.Web.Generated.Response (ResponseClass)
import GHC.Wasm.Web.Generated.Response qualified as Resp
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Humblr.Types
import Language.Javascript.JSaddle hiding (Nullable)
import Miso
import Miso.String (MisoString)
import Servant.API
import Servant.Auth.Client (Token (..))
import Servant.Client.Core
import Servant.Client.FetchAPI
import Servant.Client.Generic (genericClient)
import Streaming.ByteString qualified as Q

initialModel :: Model
initialModel =
  Model
    { mode = Idle
    , errorMessage = Nothing
    , modal = Nothing
    }

entrypointId :: MisoString
entrypointId = "app"

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
  , articles :: {-# UNPACK #-} !(Paged Article)
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
  , blobURLs :: !BlobURLs
  }
  deriving (Show, Eq, Generic)

toArticleUpdate :: T.Text -> ArticleFragment -> JSM ArticleUpdate
toArticleUpdate slug ArticleFragment {..} = do
  attachments <- toAttachments slug blobURLs

  pure ArticleUpdate {tags = F.toList tags, ..}

toAttachments :: T.Text -> BlobURLs -> JSM [Attachment]
toAttachments slug blobURLs =
  forM (F.toList blobURLs.urls) \EditedAttachment {url = origUrl, ..} -> do
    url <- case origUrl of
      TempImg url -> do
        blob <- liftIO $ await =<< js_fetch (fromText url)
        src <-
          liftIO $
            nullable (pure mempty) (Q.toStrict_ . fromReadableStream)
              =<< Resp.js_get_body blob
        newUri <- callApi $ adminAPI.postImage slug name ctype src
        pure newUri
      FixedImg url -> pure url
    pure Attachment {..}

toArticleSeed :: T.Text -> ArticleFragment -> JSM ArticleSeed
toArticleSeed slug ArticleFragment {..} = do
  attachments <- toAttachments slug blobURLs
  pure ArticleSeed {tags = F.toList tags, ..}

toArticleEdition :: Article -> ArticleFragment
toArticleEdition Article {..} =
  ArticleFragment
    { newTag = ""
    , tags = Seq.fromList tags
    , blobURLs = BlobURLs $ OM.fromList $ map ((.name) &&& fromAttachment) attachments
    , ..
    }

data EditedArticle = EditedArticle
  { original :: !Article
  , edition :: !ArticleFragment
  , viewState :: !EditViewState
  }
  deriving (Show, Eq, Generic)

data EditedAttachment = EditedAttachment
  { url :: !ImageUrl
  , ctype :: !ImageType
  , name :: !T.Text
  }
  deriving (Show, Eq, Generic)

fromEditedAttachment :: EditedAttachment -> Attachment
fromEditedAttachment EditedAttachment {..} = Attachment {url = unUrl, ..}
  where
    unUrl = case url of
      TempImg u -> u
      FixedImg u -> u

fromAttachment :: Attachment -> EditedAttachment
fromAttachment Attachment {..} = EditedAttachment {url = FixedImg url, ..}

data ImageUrl
  = TempImg !MisoString
  | FixedImg !MisoString
  deriving (Show, Eq, Generic)

attachmentUrl :: ImageSize -> ImageUrl -> MisoString
attachmentUrl sz = \case
  TempImg url -> url
  FixedImg url -> resouceUrl sz url

resouceUrl :: ImageSize -> T.Text -> T.Text
resouceUrl sz name = "/" <> toUrlPiece (rootApiLinks.images sz $ T.splitOn "/" name)

data EditViewState = Edit | Preview
  deriving (Show, Eq, Generic)

newtype TopPage = MkTopPage {articles :: Paged Article}
  deriving (Show, Generic, Eq)

newtype AdminPage = MkAdminPage {articles :: Paged Article}
  deriving (Show, Generic, Eq)

data ErrorPage = MkErrorPage
  { title :: !MisoString
  , message :: !MisoString
  }
  deriving (Show, Generic, Eq)

data Model = Model
  { mode :: !Mode
  , errorMessage :: !(Maybe ErrorMessage)
  , modal :: !(Maybe Modal)
  }
  deriving (Show, Generic, Eq)

data ShareInfo = ShareInfo {url :: !URI, title :: !T.Text, text :: !T.Text}
  deriving (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

data Modal = Share !ShareInfo
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
  | ShowTagArticles !T.Text !(Paged Article)
  | SetEditedSlug !MisoString
  | ShowErrorNotification !ErrorMessage !(Maybe Mode)
  | DismissError
  | ShowErrorPage !MisoString !MisoString
  | NoOp
  | SetFieldValue !MisoString !MisoString
  | ShareArticle !Article
  | ShowModal !Modal
  | DismissModal
  | CopyValueById !MisoString
  | DeleteArticle !MisoString
  | FileChanged !ElementId
  | AddBlobURLs !BlobURLs
  | RemoveBlobURL !ImageUrl
  deriving (Show, Generic)

newtype ElementId = ElementId {runElementId :: MisoString}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance (HasClient m ep) => HasClient m (Image :> ep) where
  type Client m (Image :> ep) = ImageType -> BS.ByteString -> Client m ep
  hoistClientMonad pm _ f cl = \a b ->
    hoistClientMonad pm (Proxy :: Proxy ep) f (cl a b)
  clientWithRoute pm _ req ct bs =
    clientWithRoute pm (Proxy :: Proxy ep) $
      setRequestBody
        (RequestBodyBS bs)
        (imageCType ct)
        req

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

slugFieldId :: MisoString
slugFieldId = "new-slug-id"

shareAreaId :: MisoString
shareAreaId = "share-message"

fileInputId :: MisoString
fileInputId = "article-file-input"

newtype BlobURLs = BlobURLs {urls :: OMap T.Text EditedAttachment}
  deriving (Show, Eq, Generic)
  deriving (Semigroup) via OM.Bias OM.R (OMap T.Text (First EditedAttachment))

instance Monoid BlobURLs where
  mempty = BlobURLs OM.empty

foreign import javascript safe "fetch($1)"
  js_fetch ::
    USVString ->
    IO (Promise ResponseClass)
