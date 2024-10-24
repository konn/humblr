{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Humblr.Migration.Tumblr.Sqlite where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Wreq

data TumblrConfig = TumblrConfig
  { clientId :: !T.Text
  , clientSecret :: !T.Text
  , redirectUri :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
