{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
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
import Data.Aeson qualified as A
import Data.Char qualified as C
import Data.Text qualified as T
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.JSON (encodeJSON)
import Humblr.Frontend.Types
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.Handler (JSHandlersClass, fetchFrom)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers (err404)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude (toUrlPiece)
import Wasm.Prelude.Linear qualified as PL

data ImagesServiceFuns = ImagesServiceFuns
  {thumb, medium, large :: T.Text -> App WorkerResponse}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '["ROOT_URI"] '[] '[ '("FRONTEND", JSHandlersClass)]

type SSRFuns = Signature SSREnv ImagesServiceFuns

type ImagesServiceClass = ServiceClass SSRFuns

type ImagesService = JSObject ImagesServiceClass

handlers :: IO ImagesService
handlers = toService @SSREnv ImagesServiceFuns {thumb, medium, large}

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
  deriving anyclass (A.ToJSON)

large :: T.Text -> App WorkerResponse
large =
  withImageOptions
    ImageOption
      { width = Just 1024
      , height = Nothing
      , metadata = Just None
      , fit = Just ScaleDown
      }

medium :: T.Text -> App WorkerResponse
medium =
  withImageOptions
    ImageOption
      { width = Just 512
      , height = Nothing
      , metadata = Just None
      , fit = Just ScaleDown
      }

thumb :: T.Text -> App WorkerResponse
thumb =
  withImageOptions
    ImageOption
      { width = Just 128
      , height = Just 128
      , metadata = Just None
      , fit = Just Contain
      }

withImageOptions :: ImageOption -> T.Text -> App WorkerResponse
withImageOptions opts path
  | T.null path = liftIO $ toWorkerResponse $ responseServerError err404
  | otherwise = do
      liftIO $ consoleLog $ fromText $ "withImageOptions: " <> T.pack (show (opts, path))
      blog <- getBinding "FRONTEND"
      root <- getEnv "ROOT_URI"
      liftIO do
        meth <- fromHaskellByteString "GET"
        imgOpts <- encodeJSON opts
        req <-
          Req.newRequest
            ( Just $
                T.intercalate
                  "/"
                  [ T.dropWhileEnd (== '/') root
                  , toUrlPiece rootApiLinks.resources
                  , T.dropWhile (== '/') path
                  ]
            )
            $ Just
            $ newDictionary
              PL.$ setPartialField "method" (nonNull meth)
              PL.. setPartialField
                "cf"
                ( nonNull
                    $ newDictionary
                      PL.$ setPartialField "image"
                    $ nonNull
                    $ nonNull
                    $ upcast imgOpts
                )
        consoleLog $ "Making request..."
        await =<< fetchFrom blog req

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()
