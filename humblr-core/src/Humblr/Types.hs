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
  ArticleSeed (..),
  ArticleUpdate (..),

  -- * APIs
  User (..),
  rootApiLinks,
  rootApiURIs,
  RootAPI (..),
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
  , -- , headArticle ::
    --     mode :- "articles" :> Capture "slug" T.Text :> Verb HEAD 200 '[PlainText] NoContent
    postArticle ::
      mode :- "articles" :> RequireUser :> ReqBody '[JSON] ArticleSeed :> Post '[JSON] NoContent
  , putArticle ::
      mode :- "articles" :> RequireUser :> Capture "slug" T.Text :> ReqBody '[JSON] ArticleUpdate :> Put '[JSON] NoContent
  , deleteArticle ::
      mode :- "articles" :> RequireUser :> Capture "slug" T.Text :> Delete '[JSON] NoContent
  , listTagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Get '[JSON] [Article]
  }
  deriving (Generic)

data ArticleUpdate = ArticleUpdate {body :: T.Text, tags :: [T.Text]}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data FrontendRoutes mode = FrontendRoutes
  { topPage :: mode :- QueryParam "page" Word :> Raw
  , articlePage :: mode :- "articles" :> Capture "slug" T.Text :> Raw
  , editArticle :: mode :- "admin" :> "edit" :> Capture "slug" T.Text :> Raw
  , newArticle :: mode :- "admin" :> "new" :> Raw
  , tagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Raw
  }
  deriving (Generic)

data ArticleSeed = ArticleSeed
  { body :: !T.Text
  , slug :: !T.Text
  , tags :: ![T.Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Article = Article
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  , tags :: ![T.Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
