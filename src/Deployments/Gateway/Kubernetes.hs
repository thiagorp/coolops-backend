module Deployments.Gateway.Kubernetes
  ( GetDeploymentLogsMonad
  , RunDeploymentMonad
  , getDeploymentLogs
  , runDeployment
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS

import Deployments.Domain.Build
import Deployments.Domain.Deployment as Deployment
import Deployments.Domain.Environment hiding (buildName)
import Deployments.Domain.Project hiding (buildName)
import qualified Kubernetes.Job as Kubernetes
import qualified Kubernetes.Pod as Kubernetes
import Util.Key

type GetDeploymentLogsMonad m
   = (Kubernetes.GetPodMonad m, Kubernetes.GetLogsMonad m)

getDeploymentLogs ::
     (GetDeploymentLogsMonad m) => Deployment.ID -> m (Maybe LBS.ByteString)
getDeploymentLogs deploymentId = do
  maybePod <- Kubernetes.getPodForJob (keyText deploymentId)
  case maybePod of
    Nothing -> return Nothing
    Just Kubernetes.Pod {..} -> Kubernetes.getLogs 20 podName

type RunDeploymentMonad m = Kubernetes.CreateJobMonad m

runDeployment ::
     (RunDeploymentMonad m) => QueuedDeployment -> DeploymentResources -> m Bool
runDeployment QueuedDeployment {..} DeploymentResources {..} =
  Kubernetes.createJob jobDescription
  where
    jobDescription =
      Kubernetes.JobDescription
        { Kubernetes.dockerImage =
            deploymentImageText (projectDeploymentImage deploymentProject)
        , Kubernetes.envVars =
            environmentEnvVars deploymentEnvironment <>
            buildParams deploymentBuild
        , Kubernetes.name = keyText deploymentId
        }
