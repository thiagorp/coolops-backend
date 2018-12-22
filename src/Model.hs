{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
  ( module Model
  , module Types
  , module Util.UUID
  , EmailAddress
  , Entity(..)
  , RawPassword
  , UTCTime
  , getCurrentTime
  , protectPassword
  ) where

import RIO

import Data.Time
import Database.Persist (Entity(..))
import Database.Persist.Postgresql.JSON (Value)
import Database.Persist.TH

import Types
import Util.EmailAddress
import Util.Password
import Util.UUID

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
BackgroundJob sql=background_jobs
  Id UUID default=uuid_generate_v4()
  name Text sqltype=varchar(255)
  params Value default='{}'::jsonb
  retryCount Int sqltype=integer
  nextRetry UTCTime Maybe
  finishedAt UTCTime Maybe
  failureReason Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

Build sql=builds
  Id UUID default=uuid_generate_v4()
  name BuildName sqltype=varchar(255)
  params (HashMap Text Text) default='{}'::jsonb
  metadata (HashMap Text Text) default='{}'::jsonb
  projectId ProjectId
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

Company sql=companies
  Id UUID default=uuid_generate_v4()
  name CompanyName sqltype=varchar(255)
  accessToken AccessToken sqltype=varchar(255)
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

Deployment sql=deployments
  Id UUID default=uuid_generate_v4()
  buildId BuildId
  environmentId EnvironmentId
  status DeploymentStatus sqltype=varchar(255)
  startedAt UTCTime Maybe sql=deployment_started_at
  finishedAt UTCTime Maybe sql=deployment_finished_at
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

Environment sql=environments
  Id UUID default=uuid_generate_v4()
  name EnvironmentName sqltype=varchar(255)
  envVars (HashMap Text Text) default='{}'::jsonb
  locked Bool default=false
  projectId ProjectId
  createdAt UTCTime
  updatedAt UTCTime
  slug Slug sqltype=varchar(255)
  deriving (Show)

Project sql=projects
  Id UUID default=uuid_generate_v4()
  name ProjectName sqltype=varchar(255)
  deploymentImage DockerImage sqltype=varchar(255)
  companyId CompanyId
  accessToken AccessToken sqltype=varchar(255)
  slug Slug sqltype=varchar(255)
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

SlackAccessToken sql=slack_access_tokens
  Id UUID default=uuid_generate_v4()
  companyId CompanyId
  teamName Text sqltype=varchar(255)
  teamId Text sqltype=varchar(255)
  scopes Text
  userAccessToken Text
  botAccessToken Text
  botUserId Text sqltype=varchar(255)
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

SlackBuildMessage sql=slack_build_messages
  Id UUID default=uuid_generate_v4()
  buildId BuildId
  slackMessageId Text sqltype=varchar(255)
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

SlackDeployment sql=slack_deployments
  Id UUID default=uuid_generate_v4()
  buildMessageId SlackBuildMessageId
  deploymentId DeploymentId
  slackUserName Text
  slackUserId Text sqltype=varchar(255)
  deployedAt UTCTime
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

SlackProjectIntegration sql=slack_project_integrations
  Id UUID default=uuid_generate_v4()
  projectId ProjectId
  channelId Text sqltype=varchar(255)
  channelName Text sqltype=varchar(255)
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)

User sql=users
  Id UUID default=uuid_generate_v4()
  firstName UserName sqltype=varchar(255)
  lastName UserName sqltype=varchar(255)
  email EmailAddress sqltype=varchar(255)
  password SafePassword sqltype=varchar(255)
  accessToken AccessToken sqltype=varchar(255)
  companyId CompanyId
  createdAt UTCTime
  updatedAt UTCTime
  deriving (Show)
|]
