module Slack.Database.Deployment
  ( createSlackDeployment
  ) where

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
