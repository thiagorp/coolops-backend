module Deployments.Gateway.Kubernetes
  ( RunDeploymentMonad
  , runDeployment
  ) where

import RIO

import Deployments.Domain.Build
import Deployments.Domain.Deployment
import Deployments.Domain.Environment hiding (buildName)
import Deployments.Domain.Project hiding (buildName)
import qualified Kubernetes.Job as Kubernetes
import Util.Key

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
