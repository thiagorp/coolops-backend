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
/projects/#Text/slack_integration CreateProjectSlackIntegrationR POST
/projects/#Text/environments ProjectsEnvironmentsR POST
/environments/#Text UpdateEnvironmentR PATCH
/builds BuildsR POST
/deployments DeploymentsR POST
/deployments/#Text/logs DeploymentLogsR GET
/slack/access_tokens CreateSlackAccessTokenR POST
/slack/messages SlackMessagesR POST
/slack/commands SlackCommandsR POST
|]
