{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Handler
  ( Handler
  , module Yesod.Core
  ) where

import App
import Yesod.Core

import Api.Routes

import Api.Handlers.CreateBuild
import Api.Handlers.CreateEnvironment
import Api.Handlers.CreateProject
import Api.Handlers.CreateProjectSlackIntegration
import Api.Handlers.CreateSlackAccessToken
import Api.Handlers.GetDeploymentLogs
import Api.Handlers.GraphQL
import Api.Handlers.HealthCheck
import Api.Handlers.Login
import Api.Handlers.Signup
import Api.Handlers.Slack.Commands
import Api.Handlers.SlackMessages
import Api.Handlers.UpdateEnvironment
import Api.Handlers.UpdateProject

mkYesodDispatch "Env" resourcesEnv

instance Yesod Env
