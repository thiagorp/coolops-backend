module Deployments.UseCases.FinishDeployment
  ( call
  ) where

import RIO

import qualified BackgroundJobs.AppJobs as Background
import Deployments.Classes
import Deployments.Domain.Deployment
import Deployments.Domain.Project (CompanyID)
import Util.Key

call ::
     (DeploymentRepo m, Background.NotifyBuildConstraint m)
  => Status
  -> CompanyID
  -> RunningDeployment
  -> m FinishedDeployment
call status companyId runningDeployment@RunningDeployment {..} = do
  finishedDeployment <- finish status runningDeployment
  saveFinishedDeployment finishedDeployment
  Background.notifyBuild companyId (keyText runningDeploymentBuildId)
  return finishedDeployment
