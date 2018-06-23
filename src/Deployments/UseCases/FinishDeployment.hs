module Deployments.UseCases.FinishDeployment
  ( call
  ) where

import RIO

import Deployments.Classes
import Deployments.Domain.Deployment

call ::
     (DeploymentRepo m, MonadIO m)
  => Status
  -> RunningDeployment
  -> m FinishedDeployment
call status runningDeployment = do
  finishedDeployment <- finish status runningDeployment
  saveFinishedDeployment finishedDeployment
  return finishedDeployment
