{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Types (
  Article (..),
  ArticleSeed (..),
  ArticleUpdate (..),

  -- * APIs
  User (..),
  rootApiLinks,
  rootApiURIs,
  RootAPI (..),
  RestApi (..),
  AdminAPI (..),
  FrontendRoutes (..),
  RequireUser,
) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Language.WASM.JSVal.Convert
import Network.Cloudflare.Worker.Binding.Service (IsServiceArg)
import Servant.API
import Servant.Auth
import Servant.Auth.JWT
import Servant.Links

type RequireUser = Auth '[CloudflareZeroTrust, JWT] User

rootApiLinks :: RootAPI (AsLink Link)
rootApiLinks = allFieldLinks

rootApiURIs :: RootAPI (AsLink URI)
rootApiURIs = allFieldLinks' linkURI

data RootAPI mode = RootAPI
  { apiRoutes :: mode :- "api" :> NamedRoutes RestApi
  , frontend :: mode :- NamedRoutes FrontendRoutes
  , assets :: mode :- "assets" :> Raw
  , resources :: mode :- "resources" :> Raw
  }
  deriving (Generic)

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

data RestApi mode = RestApi
  { listArticles ::
      mode :- "articles" :> QueryParam "page" Word :> Get '[JSON] [Article]
  , getArticle ::
      mode :- "articles" :> Capture "slug" T.Text :> Get '[JSON] Article
  , listTagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Get '[JSON] [Article]
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
  }
  deriving (Generic)

data ArticleUpdate = ArticleUpdate {body :: T.Text, tags :: [T.Text]}
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
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON ArticleSeed

data Article = Article
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  , tags :: ![T.Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving (IsServiceArg) via ViaJSON Article
