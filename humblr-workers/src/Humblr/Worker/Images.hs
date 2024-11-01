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
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Base (proxy#)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal')
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Response qualified as RawResp
import Humblr.Worker.Storage (SignParams (..), StorageServiceClass)
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service
import Network.Cloudflare.Worker.FetchAPI (fetch)
import Network.Cloudflare.Worker.Response (WorkerResponse, newResponse')
import Servant.Cloudflare.Workers (err404)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude (toUrlPiece)
import Wasm.Prelude.Linear qualified as PL

data ImagesServiceFuns = ImagesServiceFuns
  {thumb, medium, large :: T.Text -> App (Maybe WorkerResponse)}
  deriving (Generic)
  deriving anyclass (ToService SSREnv)

type App = ServiceM SSREnv '[]

type SSREnv = BindingsClass '["ROOT_URI"] '[] '[ '("STORAGE", StorageServiceClass)]

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

class Commasep a where
  commaSep :: a -> T.Text
  default commaSep ::
    (Generic a, GCommasep (Rep a)) =>
    a ->
    T.Text
  commaSep = gcommaSep . from

class GCommasep f where
  gcommaList :: f () -> [T.Text]

gcommaSep :: (GCommasep f) => f () -> T.Text
gcommaSep = T.intercalate "," . gcommaList

instance GCommasep U1 where
  gcommaList = mempty

instance (GCommasep f, GCommasep g) => GCommasep (f :*: g) where
  gcommaList (l :*: r) = gcommaList l <> gcommaList r

instance (GCommasep f) => GCommasep (D1 i f) where
  gcommaList = gcommaList . unM1

instance (GCommasep f) => GCommasep (C1 i f) where
  gcommaList = gcommaList . unM1

instance
  (A.ToJSON a, KnownSymbol l) =>
  GCommasep (S1 ('MetaSel ('Just l) z b c) (K1 i a))
  where
  gcommaList (M1 (K1 a)) = [toUrlPiece (symbolVal' @l proxy#) <> "=" <> toUrlPiece (TE.decodeUtf8 $ LBS.toStrict $ A.encode a)]

data ImageOption = ImageOption {height, width :: !(Maybe Word), metadata :: !(Maybe ImageMetadata), fit :: !(Maybe Fit)}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Commasep)

instance A.ToJSON ImageOption where
  toJSON = A.genericToJSON A.defaultOptions {A.omitNothingFields = True}

large :: T.Text -> App (Maybe WorkerResponse)
large =
  withImageOptions
    ImageOption
      { width = Just 768
      , height = Nothing
      , metadata = Just None
      , fit = Just ScaleDown
      }

medium :: T.Text -> App (Maybe WorkerResponse)
medium =
  withImageOptions
    ImageOption
      { width = Just 512
      , height = Nothing
      , metadata = Just None
      , fit = Just ScaleDown
      }

thumb :: T.Text -> App (Maybe WorkerResponse)
thumb =
  withImageOptions
    ImageOption
      { width = Just 128
      , height = Just 128
      , metadata = Just None
      , fit = Just Contain
      }

withImageOptions :: ImageOption -> T.Text -> App (Maybe WorkerResponse)
withImageOptions opts path
  | T.null path = liftIO $ fmap Just $ toWorkerResponse $ responseServerError err404
  | otherwise = do
      liftIO $ consoleLog $ fromText $ "withImageOptions: " <> T.pack (show (opts, path))
      storage <- getBinding "STORAGE"
      root <- getEnv "ROOT_URI"
      liftIO $ runMaybeT do
        url <- MaybeT $ await' =<< storage.issueSignedURL SignParams {duration = 60, name = path}
        liftIO do
          let cdn =
                T.intercalate
                  "/"
                  [ T.dropWhileEnd (== '/') root
                  , "cdn-cgi/image"
                  , commaSep opts
                  , url
                  ]
          resp <- await =<< fetch (inject $ fromText @USVStringClass cdn) none
          body <- RawResp.js_get_body resp
          stat <- RawResp.js_get_status resp
          statTxt <- RawResp.js_get_statusText resp
          headers <- RawResp.js_get_headers resp
          auto <- fromHaskellByteString "automatic"
          cf <- emptyObject
          newResponse' (inject <$> fromNullable body) $
            Just $
              newDictionary
                PL.$ setPartialField "status" (toJSPrim stat)
                PL.. setPartialField "statusText" statTxt
                PL.. setPartialField "headers" (inject headers)
                PL.. setPartialField "encodeBody" auto
                PL.. setPartialField "cf" cf

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()
