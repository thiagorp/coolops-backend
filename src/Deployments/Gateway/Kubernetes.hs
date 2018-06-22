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
     (RunDeploymentMonad m)
  => QueuedDeployment
  -> Environment
  -> Build
  -> Project
  -> m Bool
runDeployment (QueuedDeployment {..}) (Environment {..}) (Build {..}) (Project {..}) =
  Kubernetes.createJob jobDescription
  where
    jobDescription =
      Kubernetes.JobDescription
        { Kubernetes.dockerImage = deploymentImageText projectDeploymentImage
        , Kubernetes.envVars = environmentEnvVars <> buildParams
        , Kubernetes.name = keyText deploymentId
        }
