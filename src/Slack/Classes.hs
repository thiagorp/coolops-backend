module Slack.Classes where

import RIO

import Slack.Domain.BuildMessage as BuildMessage
import Slack.Domain.Deployment

class Monad m =>
      SlackBuildMessageRepo m
  where
  createSlackBuildMessage :: BuildMessage -> m ()
  getSlackBuildMessage :: BuildID -> m (Maybe BuildMessage)

class Monad m =>
      SlackDeploymentRepo m
  where
  createSlackDeployment :: Deployment -> m ()
