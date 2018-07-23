module Slack.Database.Deployment
  ( createSlackDeployment
  , getSlackDeployment
  ) where

import RIO

import Data.Time
import Database.PostgreSQL.Simple

import Common.Database
import Slack.Domain.Deployment

createSlackDeployment :: (HasPostgres m) => Deployment -> m ()
createSlackDeployment Deployment {..} = runDb' q values
  where
    q =
      "insert into slack_deployments\
        \ (id, build_message_id, deployment_id, slack_user_name, slack_user_id, deployed_at, created_at, updated_at)\
        \ values (?, ?, ?, ?, ?, ?, NOW(), NOW())"
    values =
      ( deploymentId
      , deploymentBuildMessageId
      , deploymentDeploymentId
      , deploymentSlackUserName
      , deploymentSlackUserId
      , deploymentDeployedAt)

getSlackDeployment :: (HasPostgres m) => DeploymentID -> m (Maybe Deployment)
getSlackDeployment deploymentId = do
  results <- runQuery q (Only deploymentId)
  case results of
    [] -> return Nothing
    row:_ -> return $ Just (build row)
  where
    q =
      "select id, build_message_id, deployment_id, slack_user_name, slack_user_id, deployed_at from slack_deployments\
        \ where deployment_id = ?\
        \ limit 1"

type Row = (ID, BuildMessageID, DeploymentID, Text, Text, LocalTime)

build :: Row -> Deployment
build (deploymentId, deploymentBuildMessageId, deploymentDeploymentId, deploymentSlackUserName, deploymentSlackUserId, deployedAt) =
  let deploymentDeployedAt = localTimeToUTC utc deployedAt
   in Deployment {..}
