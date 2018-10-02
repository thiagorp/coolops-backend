module Deployments.UseCases.FinishDeployment
  ( call
  ) where

import RIO

import qualified BackgroundJobs.AppJobs as Background
import Common.Database
import Deployments.Database.Deployment (saveFinishedDeployment)
import Deployments.Domain.Deployment
import Deployments.Domain.Project (CompanyID)
import Util.Key

call ::
     (HasPostgres m, Background.NotifyBuildConstraint m, Background.NotifySlackDeployerConstraint m)
  => Status
  -> CompanyID
  -> RunningDeployment
  -> m FinishedDeployment
call status companyId runningDeployment@RunningDeployment {..} = do
  finishedDeployment <- finish status runningDeployment
  saveFinishedDeployment finishedDeployment
  Background.notifyBuild companyId (keyText runningDeploymentBuildId)
  Background.notifySlackDeployer companyId (keyText runningDeploymentId)
  return finishedDeployment
