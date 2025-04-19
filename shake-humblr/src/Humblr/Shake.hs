{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Shake (rules, defaultMain) where

import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, takeTMVar)
import Control.Lens hiding ((<.>))
import Control.Monad (join, unless, void)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Foldable (sequenceA_, traverse_)
import Data.List (stripPrefix)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Text.Lens (IsText (..))
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Directory (getHomeDirectory)

ghcVersion :: String
ghcVersion = "9.12"

defaultMain :: IO ()
defaultMain = shakeArgs shakeOptions . rules =<< newTMVarIO ()

type ComponentName = String

data RawTarget = RawTarget {package, component :: !String}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

data Target = Worker !ComponentName | Frontend
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

fromRawTarget :: RawTarget -> Maybe Target
fromRawTarget RawTarget {..}
  | package == "humblr-workers" = Just $ Worker component
  | package == "humblr-frontend" && component == "humblr-frontend" = Just Frontend
  | otherwise = Nothing

data BinPath = BinPath !Target
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult BinPath = FilePath

data WasmLibPath = WasmLibPath
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult WasmLibPath = FilePath

newtype BuildPackage = BuildPackage String
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, NFData, Binary)

type instance RuleResult BuildPackage = ()

data GetWorkers = GetWorkers
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult GetWorkers = [ComponentName]

class IsTarget a where
  toRawTarget :: a -> RawTarget

instance IsTarget Target where
  {-# INLINE toRawTarget #-}
  toRawTarget = \case
    Worker component -> RawTarget {package = "humblr-workers", component}
    Frontend -> join RawTarget "humblr-frontend"

instance IsTarget RawTarget where
  {-# INLINE toRawTarget #-}
  toRawTarget = id

rawWasmPath :: (IsTarget target) => target -> FilePath
rawWasmPath (toRawTarget -> RawTarget {..}) = "_build" </> "raw-wasm" </> package </> component <.> "wasm"

optWasmPath :: (IsTarget target) => target -> FilePath
optWasmPath (toRawTarget -> RawTarget {..}) = "_build" </> "opt-wasm" </> package </> component <.> "wasm"

rawJsFFiPath :: (IsTarget target) => target -> FilePath
rawJsFFiPath (toRawTarget -> RawTarget {..}) = "_build" </> "js-ffi" </> package </> component </> "ghc_wasm_jsffi.js"

parseWasmPath :: FilePath -> Maybe Target
parseWasmPath path = do
  package : (dropExtensions -> component) : _ <- pure $ drop 2 $ splitDirectories path
  fromRawTarget RawTarget {..}

frontendJsFfi :: FilePath
frontendJsFfi = workersPath </> "router" </> "assets" </> "assets" </> "ghc_wasm_jsffi.js"

assetsJsons :: [FilePath]
assetsJsons =
  [ workersPath </> "ssr" </> "assets" </> "assets.json"
  , workersPath </> "router" </> "assets" </> "assets" </> "assets.json"
  ]

frontendWasm :: FilePath
frontendWasm = workersPath </> "router" </> "assets" </> "assets" </> "humblr-frontend.wasm"

frontendIndexJs :: FilePath
frontendIndexJs = workersPath </> "router" </> "assets" </> "assets" </> "index.js"

frontendIndexHtml :: FilePath
frontendIndexHtml = workersPath </> "router" </> "assets" </> "assets" </> "index.html"

data BuildWorker = BuildWorker !ComponentName
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult BuildWorker = ()

newtype GetFileHash = GetFileHash FilePath
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance RuleResult GetFileHash = String

type Semaphore = TMVar ()

withSemaphore :: Semaphore -> Action a -> Action a
withSemaphore sem =
  actionBracket (liftIO $ atomically $ takeTMVar sem) (liftIO . atomically . putTMVar sem) . const

rules :: Semaphore -> Rules ()
rules sem = do
  want ["all"]
  "clean" ~> do
    removeFilesAfter "_build" ["//*"]
  buildWorker <-
    (. BuildWorker) <$> addOracle \(BuildWorker comp) -> do
      let workerDir = workersPath </> fromMaybe comp (stripPrefix "humblr-" comp)
      entries <-
        getDirectoryFiles ("humblr-workers" </> "data" </> "worker-template") ["//*"]
      cfs <-
        getDirectoryFiles (wrangerConfsDirOf comp) ["wrangler.toml", ".dev.vars"]

      need
        [ workerDir </> "src" </> "worker.wasm"
        , workerDir </> "src" </> "ghc_wasm_jsffi.js"
        , workerDir </> "src" </> "worker.js"
        , workerDir </> "node_modules" </> ".package-lock.json"
        ]
        *> need [workerDir </> ch | ch <- cfs]
        *> need [workerDir </> ch | ch <- entries]
  getWasmLibPath <-
    ($ WasmLibPath) <$> addOracle \WasmLibPath -> do
      Stdout out <- commandWasm [] ("wasm32-wasi-ghc-" <> ghcVersion) ["--print-libdir"]
      pure $ out & packed %~ T.strip
  getFileHash <-
    (. GetFileHash) <$> addOracleCache \(GetFileHash path) -> do
      need [path]
      content <- liftIO $ LBS.readFile path
      let hash = showDigest $ sha1 content
      pure $ take 7 hash
  getWorkers <-
    ($ GetWorkers) <$> addOracleCache \GetWorkers -> do
      readFileLines "humblr-workers/humblr-workers.cabal"
        <&> filter (L.isPrefixOf "executable")
        <&> mapMaybe (fmap NE.last . NE.nonEmpty . words)
  getBinPath <-
    (. BinPath) <$> addOracleCache \(BinPath target) -> do
      need ["cabal-wasm.project", "cabal-wasm.project.freeze"]
      Stdout out <- cabal "list-bin" ["-v0", toCabalTarget target]
      pure $ out & packed %~ T.strip
  buildPackage <-
    (. BuildPackage) <$> addOracle \(BuildPackage pkg) -> do
      let deps = case pkg of
            "humblr-workers" -> ["humblr-frontend", "humblr-core"]
            "humblr-frontend" -> ["humblr-core"]
            _ -> []
      unless (null deps) $ void $ askOracles $ map BuildPackage deps
      need . map (pkg </>) =<< getDirectoryFiles pkg ["*.cabal", "//*.hs", "//*.lhs", "//*.hsig", "//*.js"]
      withSemaphore sem $ cabal_ "build" [pkg]

  "all" ~> do
    need ["workers", "frontend"]

  "frontend" ~> do
    assets <- getDirectoryFiles ("humblr-workers" </> "data" </> "assets") ["//*"]
    sequenceA_
      [ need assetsJsons
      , need [frontendJsFfi, frontendIndexHtml, frontendWasm, frontendIndexJs]
      , need
          [ workersPath </> "router" </> "assets" </> targ
          | targ <- assets
          ]
      ]

  "workers" ~> do
    workers <- getWorkers
    need ["frontend"] *> traverse_ buildWorker workers

  rawWasmPath (RawTarget "*" "*") %> \out -> do
    let target = fromJust $ parseWasmPath out
    buildPackage $ (toRawTarget target).package
    path <- getBinPath target
    copyFile' path out

  rawJsFFiPath (RawTarget "*" "*") %> \out -> do
    let target = fromJust $ parseWasmPath out
        rawPath = rawWasmPath target
    need [rawPath]
    libPath <- getWasmLibPath
    commandWasm_ [] (libPath </> "post-link.mjs") ["--input", rawPath, "--output", out]

  optWasmPath (RawTarget "*" "*") %> \out -> do
    let target = fromJust $ parseWasmPath out
        rawPath = rawWasmPath target
    need [rawPath]
    commandWasm_
      []
      "wizer"
      [ "--allow-wasi"
      , "--wasm-bulk-memory"
      , "true"
      , "--init-func"
      , "_initialize"
      , "-o"
      , out
      , rawPath
      ]
    commandWasm_ [] "wasm-opt" ["-Oz", out, "-o", out]
    commandWasm_ [] "wasm-tools" ["strip", "-o", out, out]

  alternatives do
    frontendJsFfi %> \out -> do
      copyFile' (rawJsFFiPath Frontend) out

    frontendWasm %> \out -> do
      need [optWasmPath Frontend]
      copyFile' (optWasmPath Frontend) out

    frontendIndexJs %> \out -> do
      -- Cache busting for WASM and FFI
      wasmSum <- getFileHash frontendWasm
      ffiSum <- getFileHash frontendJsFfi
      src <- readFile' $ "humblr-frontend" </> "data" </> "index.js"
      writeFileChanged out $
        src
          & packed
            %~ T.replace "humblr-frontend.wasm" ("humblr-frontend.wasm?" <> T.pack wasmSum)
              . T.replace "ghc_wasm_jsffi.js" ("ghc_wasm_jsffi.js?" <> T.pack ffiSum)

    frontendIndexHtml %> \out -> do
      shasum <- getFileHash frontendIndexJs
      let bustedJs = "index.js?" <> T.pack shasum
      src <- readFile' $ "humblr-frontend" </> "data" </> "index.html"
      writeFileChanged out $ src & packed %~ T.replace "index.js" bustedJs

    assetsJsons |%> \out -> do
      shasum <- getFileHash frontendIndexJs
      let bustedJs = "index.js?" <> T.pack shasum
      writeFileChanged out $ LT.unpack $ LTE.decodeUtf8 $ A.encode $ A.object ["script" A..= bustedJs]

    workersPath </> "*" </> "src" </> "worker.wasm" %> \out -> do
      let target = Worker $ "humblr-" <> splitDirectories out !! 2
      copyFile' (optWasmPath target) out

    [ workersPath </> "*" </> cfg
      | cfg <- [".dev.vars", "wrangler.toml"]
      ]
      |%> \out -> do
        let compName = "humblr-" <> splitDirectories out !! 2
        copyFile' (wrangerConfsDirOf compName </> takeFileName out) out

    workersPath </> "*" </> "src" </> "ghc_wasm_jsffi.js" %> \out -> do
      let target = Worker $ "humblr-" <> splitDirectories out !! 2
          rawFfiPath = rawJsFFiPath target
      need [rawFfiPath]
      commandWasm_
        []
        ("node")
        [ "build-scripts" </> "jsffi-patcher.mjs"
        , rawFfiPath
        , out
        ]

    workersPath </> "router" </> "assets" <//> "*" %> \out -> do
      let origPath = joinPath $ "humblr-workers" : "data" : "assets" : drop 4 (splitDirectories out)
      copyFile' origPath out

    workersPath </> "*" </> "node_modules" </> ".package-lock.json" %> \out -> do
      let workerDir = joinPath $ take 3 $ splitDirectories out
      need
        [ workerDir </> js
        | js <- ["package-lock.json", "package.json", "vitest.config.js"]
        ]
      commandWasm_ [Cwd workerDir] "npm" ["install"]

    workersPath </> "*" <//> "*" %> \out -> do
      let origPath = joinPath $ "humblr-workers" : "data" : "worker-template" : drop 3 (splitDirectories out)
      copyFile' origPath out

wrangerConfsDirOf :: ComponentName -> FilePath
wrangerConfsDirOf compName = "humblr-workers" </> "data" </> "wrangler-configs" </> compName

workersPath :: FilePath
workersPath = "_build" </> "workers"

toCabalTarget :: Target -> String
toCabalTarget (toRawTarget -> RawTarget {..}) = package <> ":exe:" <> component

getWasmPaths :: IO [FilePath]
getWasmPaths = do
  home <- getHomeDirectory
  let ghcWasm = home </> ".ghc-wasm"
  pure
    [ ghcWasm </> dir
    | dir <-
        [ "wasm-run" </> "bin"
        , "wasmtime" </> "bin"
        , "binaryen" </> "bin"
        , "nodejs" </> "bin"
        , "wasi-sdk" </> "bin"
        ]
    ]

cabal :: (HasCallStack, CmdResult r) => String -> [String] -> Action r
cabal mode args = do
  commandWasm [] "cabal" ("--project-file=cabal-wasm.project" : mode : args)

cabal_ :: (HasCallStack) => String -> [String] -> Action ()
cabal_ mode args = do
  commandWasm_ [] "cabal" ("--project-file=cabal-wasm.project" : mode : args)

commandWasm :: (HasCallStack, CmdResult r) => [CmdOption] -> String -> [String] -> Action r
commandWasm opts prog args = do
  paths <- liftIO getWasmPaths
  command (AddPath paths [] : opts) prog args

commandWasm_ :: (HasCallStack) => [CmdOption] -> String -> [String] -> Action ()
commandWasm_ = commandWasm
