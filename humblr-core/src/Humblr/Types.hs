{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Types (Article (..), ArticleSummary (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Article = Article
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  , tags :: ![T.Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ArticleSummary = ArticleSummary
  { body :: !T.Text
  , slug :: !T.Text
  , updatedAt :: !UTCTime
  , createdAt :: !UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
