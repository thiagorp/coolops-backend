{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Handler
  ( Handler
  ) where

import Env
import Yesod.Core

import Api.Routes

import Api.Handlers.ConnectProjectWithSlack
import Api.Handlers.CreateBuild
import Api.Handlers.CreateDeployment
import Api.Handlers.CreateEnvironment
import Api.Handlers.CreateProject
import Api.Handlers.GetDeploymentLogs
import Api.Handlers.GraphQL
import Api.Handlers.HealthCheck
import Api.Handlers.Login
import Api.Handlers.Signup
import Api.Handlers.SlackMessages
import Api.Handlers.UpdateEnvironment
import Api.Handlers.UpdateProject

mkYesodDispatch "Env" resourcesEnv

instance Yesod Env
