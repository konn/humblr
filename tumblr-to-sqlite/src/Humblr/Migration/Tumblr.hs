{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Migration.Tumblr (
  defaultMain,
  defaultMainWith,
  Options (..),
) where

import Control.Applicative ((<**>))
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueue, readTBMQueue, writeTBMQueue)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Loops (whileJust_)
import Data.Aeson qualified as A
import Data.CaseInsensitive qualified as CI
import Data.Generics.Labels ()
import Data.List (sort)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (TimeZone (..), ZonedTime (..), defaultTimeLocale, formatTime, zonedTimeToUTC)
import Data.Time.Format (parseTimeM)
import Data.Yaml.Aeson qualified as Y
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (concurrently_, pooledMapConcurrentlyN_)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Effectful.FileSystem
import Effectful.FileSystem.Glob (globDirFiles1)
import Effectful.FileSystem.Tagged (doesDirExist, readFileBinaryStrict, readFileTextStrict, resolveDir')
import Effectful.Log.Extra
import Effectful.Random.Static (Random, evalRandom, getStdGen, uniformR)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import GHC.Generics (Generic)
import Humblr.Frontend.Types hiding (adminAPI, api)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative qualified as Opt
import Path.Tagged
import Servant.Auth.Client
import Servant.Client
import Servant.Client.Generic
import StmContainers.Map qualified as TMap
import Text.HTML.Scalpel

defaultMain :: IO ()
defaultMain =
  defaultMainWith =<< Opt.execParser optionsP

defaultMainWith :: Options -> IO ()
defaultMainWith opts = do
  gen <- getStdGen
  runEff $ runHttp $ runConcurrent $ evalRandom gen $ runFileSystem $ runStdErrLogger "" LogInfo do
    logInfo_ $ "Reading config from: " <> T.pack opts.config
    config <- Y.decodeFileThrow @_ @AppConfig opts.config
    inputDir <- resolveDir' opts.inputDir
    there <- doesDirExist inputDir
    unless there do
      throwString $ "Input directory not found: " <> opts.inputDir
    inputs <- globDirFiles1 "*.html" (inputDir </> htmlDir)
    logInfo_ $ tshow (length inputs) <> "HTML(s) found under " <> T.pack (toFilePath inputDir)
    cap <- getNumCapabilities
    let numThs = maybe cap (\n -> if n <= 0 then cap else fromIntegral n) opts.numThreads
    logInfo_ $ "Spawning " <> tshow (length inputs) <> " threads..."
    runReader config do
      workQueue <- atomically $ newTBMQueue 512
      faileds <- atomically TMap.new
      runReader AppEnv {..} do
        mapM_ (atomically . writeTBMQueue workQueue) inputs
          `concurrently_` pooledMapConcurrentlyN_
            numThs
            (\workerId -> runReader Worker {..} runWorker)
            [0 .. numThs - 1]

optionsP :: Opt.ParserInfo Options
optionsP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Upload Tumblr articles"
  where
    p = do
      numThreads <-
        Opt.optional $
          Opt.option Opt.auto $
            Opt.long "threads"
              <> Opt.short 'j'
              <> Opt.metavar "NUM"
              <> Opt.help "# of threads (default: Number of RTS cores)"
      config <-
        Opt.strOption $
          Opt.long "config"
            <> Opt.short 'c'
            <> Opt.metavar "FILE"
            <> Opt.help "The path to the config YAML file"
      inputDir <-
        Opt.strArgument $
          Opt.metavar "DIR" <> Opt.help "Input directory"
      pure Options {..}

data Options = Options
  { numThreads :: !(Maybe Word)
  , config :: !FilePath
  , inputDir :: !FilePath
  }
  deriving (Show, Eq, Ord, Generic)

type data FileTag = InputDir | ArticleHtml | MediaDir | HtmlDir

mediaDir :: PathTo MediaDir (RelTo InputDir) Dir
mediaDir = [reldir|media|]

htmlDir :: PathTo HtmlDir (RelTo InputDir) Dir
htmlDir = [reldir|posts/html|]

data AppConfig = AppConfig
  { root :: !BaseUrl
  , clientId :: !T.Text
  , clientSecret :: !T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (A.FromJSON, A.ToJSON)

data AppEnv = AppEnv
  { inputDir :: !(PathTo InputDir Abs Dir)
  , faileds :: !(TMap.Map (PathTo ArticleHtml Abs File) SomeException)
  }
  deriving (Generic)

data ParsedArticle = ParsedArticle
  { body :: !T.Text
  , tags :: ![T.Text]
  , date :: !UTCTime
  , imgs :: !Int
  }
  deriving (Show, Generic)

data Worker = Worker
  { workQueue :: !(TBMQueue (PathTo ArticleHtml Abs File))
  , workerId :: !Int
  }
  deriving (Generic)

api :: RestApi (AsClientT ClientM)
api = (genericClient @RootAPI).apiRoutes

adminAPI :: Token -> AdminAPI (AsClientT ClientM)
adminAPI = api.adminAPI

askAdminAPI :: (Reader AppConfig :> es) => Eff es (AdminAPI (AsClientT ClientM))
askAdminAPI = do
  config <- ask @AppConfig
  pure $
    adminAPI $
      CloudflareToken $
        Just $
          ServiceToken
            { clientSecret = TE.encodeUtf8 config.clientSecret
            , clientId = TE.encodeUtf8 config.clientId
            }

runWorker ::
  ( FileSystem :> es
  , Reader AppEnv :> es
  , Concurrent :> es
  , Reader AppConfig :> es
  , Random :> es
  , Log :> es
  , Reader Worker :> es
  , Http :> es
  ) =>
  Eff es ()
runWorker = do
  Worker {..} <- ask
  localDomain ("worker #" <> tshow workerId) do
    logInfo_ "Worker started."
    whileJust_ (atomically $ readTBMQueue workQueue) \work -> do
      logInfo_ "Worker vacant. Wait up to 5secs..."
      threadDelay =<< uniformR (0_500_000, 5_000_000)
      postArticle work
    logInfo_ "Worker finished."

call :: (Reader AppConfig :> es, Http :> es) => ClientM a -> Eff es a
call act = do
  HttpRep man <- getStaticRep
  config <- ask @AppConfig
  either throwIO pure =<< unsafeEff_ (runClientM act (mkClientEnv man config.root))

data Http :: Effect

type instance DispatchOf Http = Static 'WithSideEffects

newtype instance StaticRep Http = HttpRep Manager

runHttp :: (IOE :> es) => Eff (Http ': es) a -> Eff es a
runHttp act = do
  man <- newTlsManager
  evalStaticRep (HttpRep man) act

postArticle ::
  ( FileSystem :> es
  , Reader AppEnv :> es
  , Concurrent :> es
  , Reader AppConfig :> es
  , Log :> es
  , Http :> es
  ) =>
  PathTo ArticleHtml Abs File ->
  Eff es ()
postArticle target = localDomain (T.pack $ fromString $ fromAbsFile target) $ handleAny reportFailed do
  logInfo_ $ "Start Processing: " <> tshow target
  src <- readFileTextStrict target
  postArticleSeed
    =<< processParsedArticle target
    =<< maybe
      (throwIO $ userError "ParseFailed")
      pure
      (scrapeStringLike src parseArticleM)
  logInfo_ $ "Done: " <> tshow target
  where
    reportFailed exc = do
      logAttention_ $ "Failed with exception: " <> T.pack (displayException exc)
      atomically . TMap.insert exc target =<< EffL.view @AppEnv #faileds

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

postArticleSeed ::
  ( Reader AppConfig :> es
  , Log :> es
  , Http :> es
  ) =>
  ArticleSeed ->
  Eff es ()
postArticleSeed seed = do
  logInfo_ "Posting article..."
  eps <- askAdminAPI
  void $ call $ eps.postArticle seed
  logInfo_ "Article posted!"

processParsedArticle ::
  ( Reader AppEnv :> es
  , FileSystem :> es
  , Reader AppConfig :> es
  , Log :> es
  , Http :> es
  ) =>
  PathTo ArticleHtml Abs File ->
  ParsedArticle ->
  Eff es ArticleSeed
processParsedArticle name pa = do
  logInfo_ $ "Article Parsed. Processing..."
  let createdAt = Just pa.date
      updatedAt = Nothing
      body = pa.body
      slug = T.pack $ formatTime defaultTimeLocale "%Y%m%d-%H-%M" pa.date
      tags = pa.tags
  base <- fromAbsFile . fst <$> splitExtension name
  endpoints <- askAdminAPI
  AppEnv {..} <- ask
  imgs <-
    fmap sort $
      globDirFiles1 (fromString $ base <> "*.jpg") (inputDir </> mediaDir)
        <> globDirFiles1 (fromString $ base <> "_*.jpg") (inputDir </> mediaDir)
  logInfo_ "Uploading attachemnts..."
  attachments <- forM imgs \src -> localDomain (T.pack $ toFilePath src) do
    let imageName = T.pack $ fromRelFile $ filename src
        ctype = case maybe "" (CI.mk . snd) $ splitExtension src of
          ".jpg" -> Jpeg
          ".jpeg" -> Jpeg
          ".png" -> Png
          _ -> Jpeg -- never mind
    image <- readFileBinaryStrict src
    url <- call $ endpoints.postImage slug imageName ctype image
    logInfo_ "Uploaded."
    pure Attachment {url = url, name = imageName, ctype}
  logInfo_ "Attachments all uploaded!"
  pure ArticleSeed {..}

parseArticleM :: Scraper T.Text ParsedArticle
parseArticleM = do
  body <- bodyM
  tags <- tagsM
  date <- timestampM
  imgs <- countImgsM
  pure ParsedArticle {..}

tagsM :: Scraper T.Text [T.Text]
tagsM = texts ("span" @: [hasClass "tag"])

bodyM :: Scraper T.Text T.Text
bodyM =
  T.intercalate "\n\n" <$> texts "p"

timestampM :: Scraper T.Text UTCTime
timestampM = do
  stamp <-
    T.unpack . T.replace "th" "" . T.replace "1st" "1" . T.replace "3rd" "3" . T.replace "2nd" "2"
      <$> text ("span" @: ["id" @= "timestamp"])
  fmap zonedTimeToUTC . ZonedTime
    <$> parseTimeM True defaultTimeLocale "%B %e, %Y %l:%M%P" stamp
    <*> pure TimeZone {timeZoneSummerOnly = False, timeZoneName = "GMT-4", timeZoneMinutes = -4 * 60}

countImgsM :: Scraper T.Text Int
countImgsM = length <$> htmls "img"
