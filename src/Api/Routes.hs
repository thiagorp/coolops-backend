{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Routes where

import RIO

import Yesod.Core

import Env

mkYesodData
  "Env"
  [parseRoutes|
/health HealthR GET
/graphql GraphQLR POST
/signup SignupR POST
/tokens TokensR POST
/projects ProjectsR POST
/projects/#Text UpdateProjectR PATCH
/projects/#Text/environments ProjectsEnvironmentsR POST
/projects/#Text/slack_integration ProjectsSlackIntegrationR POST
/environments/#Text UpdateEnvironmentR PATCH
/builds BuildsR POST
/deployments DeploymentsR POST
/deployments/#Text/logs DeploymentLogsR GET
/slack/messages SlackMessagesR POST
|]
