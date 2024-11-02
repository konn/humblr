{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Types (
  Article (..),
  ArticleSeed (..),
  ArticleUpdate (..),
  Image,
  ImageType (..),
  Attachment (..),
  Paged (..),
  imageCType,
  parseImageCType,
  toExtension,

  -- * APIs
  User (..),
  rootApiLinks,
  rootApiURIs,
  RootAPI (..),
  RestApi (..),
  AdminAPI (..),
  ImagesAPI (..),
  imageLink,
  ImageSize (..),
  FrontendRoutes (..),
  RequireUser,
) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins (USVStringClass, toText)
import Language.WASM.JSVal.Convert
import Network.Cloudflare.Worker.Binding.D1 (FromD1Value (..), ToD1Value (..))
import Network.Cloudflare.Worker.Binding.Service (IsServiceArg (..))
import Network.HTTP.Media (MediaType)
import Network.HTTP.Media qualified as M
import Servant.API
import Servant.Auth
import Servant.Auth.JWT
import Servant.Links
import Text.Read (readEither)

type RequireUser = Auth '[CloudflareZeroTrust, JWT] User

rootApiLinks :: RootAPI (AsLink Link)
rootApiLinks = allFieldLinks

rootApiURIs :: RootAPI (AsLink URI)
rootApiURIs = allFieldLinks' linkURI

data RootAPI mode = RootAPI
  { apiRoutes :: mode :- "api" :> NamedRoutes RestApi
  , assets :: mode :- "assets" :> Raw
  , resources ::
      mode
        :- "resources"
          :> CaptureAll "name" T.Text
          :> QueryParam' '[Required] "expiry" POSIXTime
          :> QueryParam' '[Required] "sign" T.Text
          :> Raw
  , images :: mode :- "images" :> NamedRoutes ImagesAPI
  -- ^
  -- NOTE: If we include 'ImageSize' as a 'Capture'd argument,
  -- resulting WASM binary exceeds 1000KiB...
  , frontend :: mode :- NamedRoutes FrontendRoutes
  }
  deriving (Generic)

{- |
NOTE: If we include 'ImageSize' as a 'Capture'd argument,
resulting WASM binary exceeds 1000KiB...
-}
data ImagesAPI mode = ImagesAPI
  { thumb :: mode :- "thumb" :> CaptureAll "path" T.Text :> Raw
  , medium :: mode :- "medium" :> CaptureAll "path" T.Text :> Raw
  , large :: mode :- "large" :> CaptureAll "path" T.Text :> Raw
  , ogp :: mode :- "ogp" :> CaptureAll "path" T.Text :> Raw
  }
  deriving (Generic)

imageLink :: ImageSize -> [T.Text] -> Link
imageLink sz = case sz of
  Thumb -> rootApiLinks.images.thumb
  Medium -> rootApiLinks.images.medium
  Large -> rootApiLinks.images.large
  Ogp -> rootApiLinks.images.ogp

data ImageSize = Thumb | Medium | Ogp | Large
  deriving (Show, Read, Eq, Generic, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance IsServiceArg ImageSize where
  type ServiceArg ImageSize = USVStringClass
  encodeServiceArg = pure . fromString . show
  {-# INLINE encodeServiceArg #-}
  parseServiceArg = pure . readEither . T.unpack . toText
  {-# INLINE parseServiceArg #-}

instance ToHttpApiData ImageSize where
  toUrlPiece = T.toLower . T.pack . show
  {-# INLINE toUrlPiece #-}

instance FromHttpApiData ImageSize where
  parseUrlPiece = \case
    "ogp" -> pure Ogp
    "large" -> pure Large
    "medium" -> pure Medium
    "thumb" -> pure Thumb
    sz -> Left $ "Unknown size: " <> sz <> " (must be one of: ogp, large, medium, or thumb)"
  {-# INLINE parseUrlPiece #-}

data RestApi mode = RestApi
  { listArticles ::
      mode :- "articles" :> QueryParam "page" Word :> Get '[JSON] (Paged Article)
  , getArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> Get '[JSON] Article
  , listTagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Get '[JSON] (Paged Article)
  , adminAPI :: mode :- "admin" :> RequireUser :> NamedRoutes AdminAPI
  }
  deriving (Generic)

data AdminAPI mode = AdminAPI
  { postArticle ::
      mode :- "articles" :> ReqBody '[JSON] ArticleSeed :> Post '[JSON] NoContent
  , putArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> ReqBody '[JSON] ArticleUpdate :> Put '[JSON] NoContent
  , deleteArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> Delete '[JSON] NoContent
  , putImage :: mode :- PutResource 'PUT
  , postImage :: mode :- PutResource 'POST
  }
  deriving (Generic)

type PutResource meth =
  "resources"
    :> Capture "slug" T.Text
    :> Capture "name" T.Text
    :> Image
    :> Verb meth 200 '[PlainText] T.Text

data User = User {email :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON User

instance ToJWT User where
  encodeJWT User {..} =
    mempty @ClaimsSet
      & #unregisteredClaims .~ Map.singleton "email" (J.toJSON email)
  {-# INLINE encodeJWT #-}

instance FromJWT User where
  decodeJWT claims =
    fmap User . eitherResult . J.fromJSON
      =<< maybe
        (Left $ "Missing 'email' claim")
        Right
        (Map.lookup "email" claims.unregisteredClaims)
  {-# INLINE decodeJWT #-}

eitherResult :: J.Result a -> Either T.Text a
eitherResult (J.Success a) = Right a
eitherResult (J.Error e) = Left $ T.pack e

instance (HasLink api) => HasLink (Image :> api) where
  type MkLink (Image :> api) x = MkLink api x
  toLink toA _ = toLink toA (Proxy @api)
  {-# INLINE toLink #-}

data Image

data ImageType = Png | Jpeg
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

toExtension :: ImageType -> T.Text
toExtension Png = ".png"
toExtension Jpeg = ".jpg"

instance ToD1Value ImageType where
  toD1ValueView Png = toD1ValueView ("image/png" :: T.Text)
  toD1ValueView Jpeg = toD1ValueView ("image/jpeg" :: T.Text)

instance FromD1Value ImageType where
  parseD1ValueView v = do
    txt <- parseD1ValueView v
    case T.toLower txt of
      "image/png" -> pure Png
      "image/jpeg" -> pure Jpeg
      _ -> Left $ "Invalid image type: " <> T.unpack txt

imageCType :: ImageType -> MediaType
imageCType Png = "image" M.// "png"
imageCType Jpeg = "image" M.// "jpeg"

parseImageCType :: T.Text -> Maybe ImageType
parseImageCType "image/png" = Just Png
parseImageCType "image/jpeg" = Just Jpeg
parseImageCType _ = Nothing

data ArticleUpdate = ArticleUpdate {body :: T.Text, tags :: [T.Text], attachments :: [Attachment], createdAt :: Maybe UTCTime, updatedAt :: Maybe UTCTime}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON ArticleUpdate

-- FIXME: admin frontends must be guarded behind a RequireUser.
-- Currently, miso doesn't support adding unsupported middleware to Route,
-- so we must make sure the endpoint is guarded behind ZeroTrust.
data FrontendRoutes mode = FrontendRoutes
  { articlePage :: mode :- "articles" :> Capture "slug" T.Text :> Raw
  , tagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Raw
  , editArticle :: mode :- "admin" :> "edit" :> Capture "slug" T.Text :> Raw
  , newArticle :: mode :- "admin" :> "new" :> Raw
  , adminHome :: mode :- "admin" :> QueryParam "page" Word :> Raw
  , topPage :: mode :- QueryParam "page" Word :> Raw
  }
  deriving (Generic)

data ArticleSeed = ArticleSeed
  { body :: !T.Text
  , slug :: !T.Text
  , tags :: ![T.Text]
  , attachments :: ![Attachment]
  , createdAt :: !(Maybe UTCTime)
  , updatedAt :: !(Maybe UTCTime)
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON ArticleSeed

data Attachment = Attachment {name :: !T.Text, url :: !T.Text, ctype :: !ImageType}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON Attachment

data Article = Article
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  , tags :: ![T.Text]
  , attachments :: ![Attachment]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON Article

data Paged a = Paged
  { payload :: !(V.Vector a)
  , page :: !Word
  , offset :: !Word
  , total :: !Word
  , hasNext :: !Bool
  }
  deriving stock (Eq, Generic, Show, Foldable, Traversable, Functor)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON (Paged a)
