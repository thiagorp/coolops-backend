module Slack.Classes where

import RIO

import Auth.Domain (CompanyID)
import Slack.Domain.BuildMessage as BuildMessage
import Slack.Domain.Deployment
import Slack.Domain.Team

class Monad m =>
      SlackTeamRepo m
  where
  createSlackTeam :: Team -> m ()
  getSlackTeamForCompany :: CompanyID -> m (Maybe Team)
  getSlackTeam :: Text -> m (Maybe Team)
  deleteSlackTeam :: Team -> m ()

class Monad m =>
      SlackBuildMessageRepo m
  where
  createSlackBuildMessage :: BuildMessage -> m ()
  getSlackBuildMessage :: BuildID -> m (Maybe BuildMessage)

class Monad m =>
      SlackDeploymentRepo m
  where
  createSlackDeployment :: Deployment -> m ()
