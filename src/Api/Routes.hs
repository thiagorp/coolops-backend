{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Routes where

import Yesod.Core

import Env
import Util.UUID

mkYesodData
  "Env"
  [parseRoutes|
/health HealthR GET
/graphql GraphQLR POST
/signup SignupR POST
/tokens TokensR POST
/projects ProjectsR POST
/projects/#UUID UpdateProjectR PATCH
/projects/#UUID/slack_integration CreateProjectSlackIntegrationR POST
/projects/#UUID/environments ProjectsEnvironmentsR POST
/environments/#UUID UpdateEnvironmentR PATCH
/builds BuildsR POST
/deployments DeploymentsR POST
/deployments/#UUID/logs DeploymentLogsR GET
/slack/access_tokens CreateSlackAccessTokenR POST
/slack/messages SlackMessagesR POST
/slack/commands SlackCommandsR POST
|]