module Slack.Database.Deployment
  ( getSlackDeployment
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import App
import Common.PersistDatabase
import Model

getSlackDeployment :: DeploymentId -> App (Maybe (Entity SlackDeployment))
getSlackDeployment deploymentId =
  selectFirst $
  from $ \d -> do
    where_ (d ^. SlackDeploymentDeploymentId ==. val deploymentId)
    return d
