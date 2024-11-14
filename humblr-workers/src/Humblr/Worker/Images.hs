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

import Control.Exception (Exception (..))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as A
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char qualified as C
import Data.String (fromString)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Headers (js_iter_Headers_ByteString_ByteString)
import GHC.Wasm.Web.Generated.RequestInfo (RequestInfo)
import GHC.Wasm.Web.Generated.RequestInit (RequestInitClass)
import GHC.Wasm.Web.Generated.Response (Response, ResponseClass, js_get_body, js_get_status, js_get_statusText)
import GHC.Wasm.Web.Generated.Response qualified as RawResp
import GHC.Wasm.Web.JSON (encodeJSON)
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Humblr.Frontend.Types (ImageSize (..))
import Humblr.Worker.Storage (SignParams (..), StorageServiceClass)
import Humblr.Worker.Utils
import Lens.Family.Total
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.Service (
  ServiceClass,
  ServiceM,
  ToService (..),
  getBinding,
 )
import Network.Cloudflare.Worker.FetchAPI (fetch)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers (ServerError (..), err404, err500)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S

data ImagesServiceFuns = ImagesServiceFuns
  { get :: ImageSize -> [T.Text] -> App WorkerResponse
  }
  deriving (Generic)

deriving anyclass instance
  (imageEnv ~ ImagesEnv) =>
  ToService imageEnv ImagesServiceFuns

type App = ServiceM ImagesEnv '[]

type ImagesEnv = BindingsClass '["ROOT_URI"] '[] '[ '("STORAGE", StorageServiceClass)]

type ImagesFuns = Signature ImagesEnv ImagesServiceFuns

type ImagesServiceClass = ServiceClass ImagesFuns

type ImagesService = JSObject ImagesServiceClass

handlers :: IO ImagesService
handlers = toService @ImagesEnv ImagesServiceFuns {get}

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
toImageOption Twitter =
  ImageOption
    { width = Just 1200
    , height = Just 628
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
            consoleLog $ "Encoded."
            skimJSON cf
            resl <- fetchWith (inject $ fromText @USVStringClass url) (nonNull $ unsafeCast cf)
            case resl of
              Left e -> do
                consoleLog $ fromString $ "Error during fetch: " <> show e
                toWorkerResponse $ responseServerError err500 {errBody = "Storage connection failure: " <> fromString (displayException e)}
              Right res -> do
                consoleLog "Response successfully attained!"
                pure $ unsafeCast res

foreign import javascript unsafe "console.log(JSON.stringify($1))"
  skimJSON :: JSObject e -> IO ()

data FetchError
  = InvariantViolation String
  | StatusError !Word16 !BS.ByteString !BS.ByteString ![(BS.ByteString, BS.ByteString)]
  | UnknownError String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

fetchWith ::
  RequestInfo ->
  Nullable RequestInitClass ->
  IO (Either FetchError Response)
fetchWith reqInfo mReqInit = do
  -- NOTE: We once used newDicationary, but it seems its purity makes GHC optimiser
  -- work wrong and makes consecutive calls to reuse body from the previous request.
  -- This must not be the case, so we provide a direct reqinit construction to avoid
  -- the bug.
  res <- await =<< js_handle_fetch =<< fetch reqInfo mReqInit
  resl <- getDictField "result" res
  resl
    & ( _case
          & onEnum' #ok do
            mresp <- fromNullable <$> getDictField "response" res
            case mresp of
              Nothing -> pure $ Left $ InvariantViolation "ok returned, but got no response!"
              Just resp -> pure $ Right resp
          & onEnum' #statusError do
            mresp <- fromNullable <$> getDictField "response" res
            case mresp of
              Just resp -> do
                status <- js_get_status resp
                msg <- toHaskellByteString =<< js_get_statusText resp
                hdrs <-
                  mapM (bitraverse toHaskellByteString toHaskellByteString)
                    =<< nullable
                      mempty
                      ( S.toList_ . fromPairIterable
                          <=< js_iter_Headers_ByteString_ByteString
                          <=< RawResp.js_get_headers
                      )
                    =<< getDictField "response" res
                body <- nullable mempty (Q.toStrict_ . fromReadableStream) =<< js_get_body resp
                pure $ Left $ StatusError status msg body hdrs
              Nothing -> pure $ Left $ InvariantViolation "statusError returned, but got no response!"
          & onEnum' #error do
            Left
              . UnknownError
              . ("UnknownError during fetch: " <>)
              . nullable "(No Message)" (T.unpack . toText)
              <$> getDictField "message" res
      )

type FetchResultFields =
  '[ '("result", EnumClass '["ok", "statusError", "error"])
   , '("response", NullableClass ResponseClass)
   , '("message", NullableClass USVStringClass)
   ]

type FetchResultClass = JSDictionaryClass FetchResultFields

foreign import javascript safe "try { const resp = await $1; if (resp.ok) { return {result: 'ok', response: resp, message: null } } else { return {result:  'statusError', response: resp, message: resp.statusText} } } catch (error) { return {result: 'error', message: error.toString(), response: null } }"
  js_handle_fetch ::
    Promise ResponseClass ->
    IO (Promise FetchResultClass)
