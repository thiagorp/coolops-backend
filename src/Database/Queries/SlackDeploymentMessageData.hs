module Database.Queries.SlackDeploymentMessageData
  ( module Common.PersistDatabase
  , module Model
  , MessageData(..)
  , getSlackDeploymentMessageData
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

data MessageData = MessageData
  { dataBuild :: !(Entity Build)
  , dataEnvironment :: !(Entity Environment)
  , dataProject :: !(Entity Project)
  , dataDeployment :: !(Entity Deployment)
  , dataSlackDeployment :: !(Entity SlackDeployment)
  , dataSlackAccessToken :: !(Entity SlackAccessToken)
  }

getSlackDeploymentMessageData :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe MessageData)
getSlackDeploymentMessageData cId dId = do
  maybeData <-
    selectFirst $
    from $ \(d `InnerJoin` b `InnerJoin` e `InnerJoin` p `InnerJoin` sd `InnerJoin` sa) -> do
      on ((p ^. ProjectCompanyId) ==. (sa ^. SlackAccessTokenCompanyId))
      on ((d ^. DeploymentId) ==. (sd ^. SlackDeploymentDeploymentId))
      on ((p ^. ProjectId) ==. (e ^. EnvironmentProjectId))
      on ((e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId))
      on ((b ^. BuildId) ==. (d ^. DeploymentBuildId))
      where_ (p ^. ProjectCompanyId ==. val cId)
      where_ (d ^. DeploymentId ==. val (DeploymentKey dId))
      where_ (d ^. DeploymentStatus `notIn` valList [Running, Queued])
      limit 1
      return (b, e, p, d, sd, sa)
  case maybeData of
    Nothing -> return Nothing
    Just (dataBuild, dataEnvironment, dataProject, dataDeployment, dataSlackDeployment, dataSlackAccessToken) ->
      return $ Just MessageData {..}
