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
import Language.Javascript.JSaddle (JSM)
import Miso hiding (defaultApp)
import Servant.Auth.Client ()

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp = miso \url ->
  App
    { subs = [uriSub HandleUrl]
    , view = viewModel
    , initialAction = Just $ StartWithUrl url
    , styles = []
    , ..
    }
  where
    model = initialModel
    update = updateModel
    events = defaultEvents
    mountPoint = Nothing
    logLevel = Off
