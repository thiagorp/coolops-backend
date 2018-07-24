module Database.Queries.SlackDeploymentMessageData
  ( MessageData(..)
  , DbStatus(..)
  , getSlackDeploymentMessageData
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Database.Deployment (DbStatus(..), notFinishedStatuses)
import Deployments.Domain.Project (CompanyID)

data MessageData = MessageData
  { dataBuildName :: !Text
  , dataEnvironmentName :: !Text
  , dataProjectName :: !Text
  , dataSendingParams :: !(Text, Text)
  , dataDbStatus :: !DbStatus
  }

getSlackDeploymentMessageData ::
     HasPostgres m => CompanyID -> Text -> m (Maybe MessageData)
getSlackDeploymentMessageData cId dId = do
  results <- runQuery q (cId, dId, In notFinishedStatuses)
  case results of
    [] -> return Nothing
    row:_ -> return $ Just (build row)
  where
    q =
      "select b.name, e.name, p.name, sd.slack_user_id, st.bot_access_token, d.status\
        \ from deployments d\
        \ join builds b on d.build_id = b.id\
        \ join environments e on d.environment_id = e.id\
        \ join projects p on e.project_id = p.id\ 
        \ join slack_deployments sd on d.id = sd.deployment_id\
        \ join slack_teams st on p.company_id = st.company_id\
        \ where p.company_id = ? and d.id = ? and d.status not in ?\
        \ limit 1"

type Row = (Text, Text, Text, Text, Text, DbStatus)

build :: Row -> MessageData
build (dataBuildName, dataEnvironmentName, dataProjectName, userId, accessToken, dataDbStatus) =
  let dataSendingParams = (userId, accessToken)
   in MessageData {..}
