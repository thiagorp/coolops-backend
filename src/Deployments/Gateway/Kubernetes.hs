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
import Deployments.Domain.Environment
import Deployments.Domain.Project
import qualified Kubernetes.Job as K8s
import qualified Kubernetes.Pod as K8s
import Util.Key

type GetDeploymentLogsMonad m = (K8s.GetPodMonad m, K8s.GetLogsMonad m)

getDeploymentLogs :: (GetDeploymentLogsMonad m) => Maybe Int -> Deployment.ID -> m (Maybe LBS.ByteString)
getDeploymentLogs nLines deploymentId = do
  maybePod <- K8s.getPodForJob (keyText deploymentId)
  case maybePod of
    Nothing -> return Nothing
    Just pod@K8s.Pod {..} ->
      case K8s.getPodContainerState K8s.deploymentContainerName pod of
        Nothing -> return Nothing
        Just (K8s.Waiting _) -> return Nothing
        _ -> K8s.getLogs nLines podName

type RunDeploymentMonad m = K8s.CreateJobMonad m

runDeployment :: (RunDeploymentMonad m) => QueuedDeployment -> DeploymentResources -> m Bool
runDeployment QueuedDeployment {..} DeploymentResources {..} = K8s.createJob jobDescription
  where
    jobDescription =
      K8s.JobDescription
        { K8s.dockerImage = deploymentImageText (projectDeploymentImage deploymentProject)
        , K8s.envVars = environmentEnvVars deploymentEnvironment <> buildParams deploymentBuild
        , K8s.name = keyText deploymentId
        }
