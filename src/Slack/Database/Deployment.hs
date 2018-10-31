module Slack.Database.Deployment
  ( module Common.PersistDatabase
  , module Model
  , getSlackDeployment
  ) where

import RIO hiding ((^.))

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

getSlackDeployment :: (HasDb m) => DeploymentId -> Db m (Maybe (Entity SlackDeployment))
getSlackDeployment deploymentId =
  selectFirst $
  from $ \d -> do
    where_ (d ^. SlackDeploymentDeploymentId ==. val deploymentId)
    return d
