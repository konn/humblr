{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Frontend (defaultMain, defaultApp) where

import Data.Generics.Labels ()
import Humblr.Frontend.Actions
import Humblr.Frontend.Types
import Humblr.Frontend.View (viewModel)
import Language.Javascript.JSaddle.Runner qualified as Runner
import Miso
import Servant.Auth.Client ()

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp = do
  url <- getCurrentURI
  let model = initialModel
  startApp
    App
      { subs = [uriSub HandleUrl]
      , view = viewModel
      , initialAction = HandleUrl url
      , ..
      }
  where
    update = updateModel
    events = defaultEvents
    mountPoint = Just "app"
    logLevel = Off
