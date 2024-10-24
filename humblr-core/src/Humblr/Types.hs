{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Types (
  Article (..),
  ArticleSummary (..),

  -- * APIs
  User (..),
  rootApiLinks,
  rootApiURIs,
  RootAPI (..),
  AdminAPI (..),
  FrontendRoutes (..),
  RequireUser,
  HTML,
) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Media qualified as M
import Servant.API
import Servant.Auth
import Servant.Auth.JWT
import Servant.Links

type RequireUser = Auth '[CloudflareZeroTrust, JWT] User

data HTML

instance Accept HTML where
  contentTypes _ =
    "text"
      M.// "html"
      M./: ("charset", "utf-8")
      NE.:| ["text" M.// "html"]

instance MimeRender HTML LBS.ByteString where
  mimeRender _ = id

instance MimeUnrender HTML LBS.ByteString where
  mimeUnrender _ = Right

instance MimeRender HTML BS.ByteString where
  mimeRender _ = LBS.fromStrict

instance MimeUnrender HTML BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict

rootApiLinks :: RootAPI (AsLink Link)
rootApiLinks = allFieldLinks

rootApiURIs :: RootAPI (AsLink URI)
rootApiURIs = allFieldLinks' linkURI

data RootAPI mode = RootAPI
  { apiRoutes :: mode :- "api" :> NamedRoutes AdminAPI
  , frontend :: mode :- NamedRoutes FrontendRoutes
  , assets :: mode :- "assets" :> Raw
  , resources :: mode :- "resources" :> Raw
  }
  deriving (Generic)

data User = User {email :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

data AdminAPI mode = AdminAPI
  { listArticles ::
      mode :- "articles" :> QueryParam "page" Word :> Get '[JSON] [Article]
  , getArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> Get '[JSON] Article
  , headArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> Verb HEAD 200 '[PlainText] NoContent
  , postArticle ::
      mode :- "articles" :> RequireUser :> ReqBody '[JSON] Article :> Post '[JSON] Article
  , deleteArticle ::
      mode :- "articles" :> RequireUser :> Capture "slug" T.Text :> Delete '[JSON] ()
  , putArticle ::
      mode :- "articles" :> RequireUser :> Capture "slug" T.Text :> ReqBody '[JSON] Article :> Put '[JSON] Article
  , listTags :: mode :- "tags" :> Get '[JSON] [T.Text]
  , listTagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Get '[JSON] [Article]
  }
  deriving (Generic)

data FrontendRoutes mode = FrontendRoutes
  { topPage :: mode :- QueryParam "page" Word :> Raw
  , articlePage :: mode :- "articles" :> Capture "slug" T.Text :> Raw
  , editArticle :: mode :- "admin" :> "edit" :> Capture "slug" T.Text :> Raw
  , newArticle :: mode :- "admin" :> "new" :> Raw
  , tagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Raw
  }
  deriving (Generic)

data Article = Article
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  , tags :: ![T.Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ArticleSummary = ArticleSummary
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
