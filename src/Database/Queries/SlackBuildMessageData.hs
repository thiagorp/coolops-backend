module Database.Queries.SlackBuildMessageData
  ( module Model
  , module Common.PersistDatabase
  , MessageData(..)
  , getSlackBuildMessageData
  ) where

import RIO hiding ((^.), isNothing, on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

type MessageBuildInfo = (Entity Build, Entity Project, Maybe (Entity SlackBuildMessage))

type MessageDeployment = (Entity SlackDeployment, Entity Environment, Entity Deployment)

data MessageData = MessageData
  { dataBuildInfo :: !MessageBuildInfo
  , dataDeployments :: ![MessageDeployment]
  , dataEnvironments :: ![Entity Environment]
  }

getSlackBuildMessageData :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe MessageData)
getSlackBuildMessageData cId bId = do
  maybeInfo <- getBuildInfo cId bId
  case maybeInfo of
    Nothing -> return Nothing
    Just info -> do
      deployments <- getDeployments info
      environments <- getEnvironments info
      return $
        Just $ MessageData
          { dataBuildInfo = info
          , dataDeployments = deployments
          , dataEnvironments = environments
          }

getBuildInfo :: (MonadIO m) => CompanyId -> UUID -> Db m (Maybe MessageBuildInfo)
getBuildInfo cId bId =
  selectFirst $
    from $ \(b `InnerJoin` p `LeftOuterJoin` sbm) -> do
      on $ just (b ^. BuildId) ==. (sbm ?. SlackBuildMessageBuildId)
      on $ (p ^. ProjectId) ==. (b ^. BuildProjectId)
      where_ $ p ^. ProjectCompanyId ==. val cId
      where_ $ b ^. BuildId ==. val (BuildKey bId)
      return (b, p, sbm)

getDeployments :: (MonadIO m) => MessageBuildInfo -> Db m [MessageDeployment]
getDeployments (_, _, maybeSlackBuildMessage) =
  case maybeSlackBuildMessage of
    Nothing -> return []
    Just (Entity sbmId _) ->
      select $
        from $ \(sd `InnerJoin` d `InnerJoin` e) -> do
          on $ (e ^. EnvironmentId) ==. (d ^. DeploymentEnvironmentId)
          on $ (d ^. DeploymentId) ==. (sd ^. SlackDeploymentDeploymentId)
          where_ $ sd ^. SlackDeploymentBuildMessageId ==. val sbmId
          orderBy [asc (d ^. DeploymentCreatedAt)]
          return (sd, e, d)

getEnvironments :: (MonadIO m) => MessageBuildInfo -> Db m [Entity Environment]
getEnvironments (_, Entity pId _, _) =
  select $
    from $ \e -> do
      where_ $ e ^. EnvironmentProjectId ==. val pId
      return e
