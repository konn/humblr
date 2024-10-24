{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Media qualified as M
import Servant.API
import Servant.Auth
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
  }
  deriving (Generic)

data User = User {email :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
  { topPage :: mode :- QueryParam "page" Word :> Get '[HTML] LBS.ByteString
  , articlePage :: mode :- "articles" :> Capture "slug" T.Text :> Get '[HTML] LBS.ByteString
  , editArticle :: mode :- "admin" :> "edit" :> Capture "slug" T.Text :> Get '[HTML] LBS.ByteString
  , newArticle :: mode :- "admin" :> "new" :> Get '[HTML] LBS.ByteString
  , tagArticles :: mode :- "tags" :> Capture "tag" T.Text :> QueryParam "page" Word :> Get '[HTML] LBS.ByteString
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
