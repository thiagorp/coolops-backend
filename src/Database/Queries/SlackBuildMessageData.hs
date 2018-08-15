module Database.Queries.SlackBuildMessageData
  ( MessageData(..)
  , Environment(..)
  , SlackDeployment(..)
  , DbStatus(..)
  , getSlackBuildMessageData
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Data.Time
import Data.UUID
import Deployments.Database.Deployment (DbStatus(..))
import Deployments.Domain.Project (CompanyID)

data Environment = Environment
  { environmentId :: !UUID
  , environmentName :: !Text
  }

data SlackDeployment = SlackDeployment
  { deploymentId :: !UUID
  , deploymentUserId :: !Text
  , deploymentTime :: !UTCTime
  , deploymentStatus :: !DbStatus
  , deploymentEnvironmentName :: !Text
  }

data MessageData = MessageData
  { dataBuildId :: !UUID
  , dataBuildName :: !Text
  , dataProjectName :: !Text
  , dataSlackDeployments :: ![SlackDeployment]
  , dataEnvironments :: ![Environment]
  }

getSlackBuildMessageData ::
     HasPostgres m => CompanyID -> Text -> m (Maybe MessageData)
getSlackBuildMessageData cId bId = do
  dataRows <- runQuery q (cId, bId)
  case dataRows of
    [] -> return Nothing
    row:_ -> Just <$> buildData row
  where
    q =
      "select b.id, b.name, p.id, p.name, sbm.id\
        \ from builds b\
        \ join projects p on p.id = b.project_id\
        \ left join slack_build_messages sbm on sbm.build_id = b.id\
        \ where p.company_id = ? and b.id = ?"

type DataRow = (UUID, Text, UUID, Text, Maybe UUID)

buildData :: HasPostgres m => DataRow -> m MessageData
buildData (dataBuildId, dataBuildName, projectId, dataProjectName, maybeSlackBuildMessageId) = do
  dataEnvironments <- getEnvironments (toText projectId)
  dataSlackDeployments <-
    maybe (return []) getSlackDeployments maybeSlackBuildMessageId
  return MessageData {..}

getEnvironments :: HasPostgres m => Text -> m [Environment]
getEnvironments projectId = do
  rows <- runQuery q (Only projectId)
  return $ map buildEnvironment rows
  where
    q = "select id, name from environments where project_id = ?"

type EnvironmentRow = (UUID, Text)

buildEnvironment :: EnvironmentRow -> Environment
buildEnvironment (environmentId, environmentName) = Environment {..}

getSlackDeployments :: HasPostgres m => UUID -> m [SlackDeployment]
getSlackDeployments buildMessageId = do
  rows <- runQuery q (Only buildMessageId)
  return $ map buildSlackDeployment rows
  where
    q =
      "select d.id, sd.slack_user_id, sd.deployed_at, d.status, e.name\
        \ from slack_deployments sd\
        \ join deployments d on d.id = sd.deployment_id\
        \ join environments e on e.id = d.environment_id\
        \ where sd.build_message_id = ?"

type SlackDeploymentRow = (UUID, Text, LocalTime, DbStatus, Text)

buildSlackDeployment :: SlackDeploymentRow -> SlackDeployment
buildSlackDeployment (deploymentId, deploymentUserId, localTime, deploymentStatus, deploymentEnvironmentName) =
  let deploymentTime = localTimeToUTC utc localTime
   in SlackDeployment {..}
