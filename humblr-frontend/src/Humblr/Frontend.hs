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
import Miso hiding (view)
import Servant.Auth.Client ()

defaultMain :: IO ()
defaultMain = defaultApp

defaultApp :: IO ()
defaultApp =
  miso defaultEvents \url ->
    (component initialModel updateModel $ \() -> viewModel)
      { subs = [uriSub HandleUrl]
      , mount = Just $ StartWithUrl url
      }
