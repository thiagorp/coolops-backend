module Database.Queries.DeploymentForLogs
  ( DbStatus(..)
  , Deployment(..)
  , getDeployment
  , isFinished
  ) where

import RIO

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Common.Database
import Deployments.Database.Deployment (DbStatus(..), isFinished)
import Deployments.Domain.Deployment

data Deployment = Deployment
  { deploymentId :: !ID
  , deploymentStatus :: !DbStatus
  }

instance FromRow Deployment where
  fromRow = do
    deploymentId <- field
    deploymentStatus <- field
    return Deployment {..}

getDeployment :: HasPostgres m => Text -> m (Maybe Deployment)
getDeployment deploymentId = do
  deployments <- runQuery q (Only deploymentId)
  case deployments of
    [] -> return Nothing
    deployment:_ -> return $ Just deployment
  where
    q = "select d.id, d.status from deployments d where d.id = ?"
