{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Instances
  ( Handler
  ) where

import Env
import Yesod.Core

import Api.Routes (Handler)

instance HasEnv Handler where
  getEnv = getYesod
