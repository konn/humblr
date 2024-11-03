{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Worker.Images (ImagesServiceClass, JSObject (..), handlers, ImagesService) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char qualified as C
import Data.String (fromString)
import Data.Text qualified as T
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.JSON (encodeJSON)
import Humblr.Frontend.Types (ImageSize (..))
import Humblr.Worker.Storage (SignParams (..), StorageServiceClass)
import Humblr.Worker.Utils
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.FetchAPI (fetch)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers (err404)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)

data ImagesServiceFuns = ImagesServiceFuns
  {get :: ImageSize -> [T.Text] -> App WorkerResponse}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '["ROOT_URI"] '[] '[ '("STORAGE", StorageServiceClass)]

type SSRFuns = Signature SSREnv ImagesServiceFuns

type ImagesServiceClass = ServiceClass SSRFuns

type ImagesService = JSObject ImagesServiceClass

handlers :: IO ImagesService
handlers = toService @SSREnv ImagesServiceFuns {get}

data Fit = ScaleDown | Contain | Cover | Crop | Pad
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON Fit where
  toJSON =
    A.genericToJSON
      A.defaultOptions
        { A.tagSingleConstructors = False
        , A.constructorTagModifier = A.camelTo2 '-'
        , A.allNullaryToStringTag = True
        }

data ImageMetadata = None | Keep | Copyright
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON ImageMetadata where
  toJSON =
    A.genericToJSON
      A.defaultOptions
        { A.tagSingleConstructors = False
        , A.constructorTagModifier = map C.toLower
        , A.allNullaryToStringTag = True
        }

data ImageOption = ImageOption {height, width :: !(Maybe Word), metadata :: !(Maybe ImageMetadata), fit :: !(Maybe Fit)}
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON ImageOption where
  toJSON = A.genericToJSON A.defaultOptions {A.omitNothingFields = True}

toImageOption :: ImageSize -> ImageOption
toImageOption Thumb =
  ImageOption
    { width = Just 256
    , height = Just 256
    , metadata = Just None
    , fit = Just ScaleDown
    }
toImageOption Medium =
  ImageOption
    { width = Just 512
    , height = Nothing
    , metadata = Just None
    , fit = Just ScaleDown
    }
toImageOption Large =
  ImageOption
    { width = Just 1024
    , height = Nothing
    , metadata = Just None
    , fit = Just ScaleDown
    }
toImageOption Ogp =
  ImageOption
    { width = Just 1200
    , height = Just 630
    , metadata = Just None
    , fit = Just Cover
    }

get :: ImageSize -> [T.Text] -> App WorkerResponse
get = withImageOptions . toImageOption

withImageOptions :: ImageOption -> [T.Text] -> App WorkerResponse
withImageOptions opts paths
  | null paths = liftIO $ toWorkerResponse $ responseServerError err404
  | otherwise = do
      storage <- getBinding "STORAGE"
      liftIO $
        maybe (toWorkerResponse $ responseServerError err404) pure =<< runMaybeT do
          liftIO $ consoleLog $ fromString $ "Issueing URL for " <> show opts <> "..."
          url <- MaybeT $ await' =<< storage.issueSignedURL SignParams {duration = 60, ..}
          liftIO $ consoleLog $ fromText $ "URL attained: " <> url
          let cfObj = A.object ["cf" A..= A.object ["image" A..= opts]]
          liftIO $ consoleLog $ fromString $ LBS8.unpack $ A.encode cfObj
          liftIO do
            cf <- encodeJSON cfObj
            unsafeCast <$> fetch (inject $ fromText @USVStringClass url) (nonNull $ unsafeCast cf)
