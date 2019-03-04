{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Deployments.Gateway.Kubernetes
  ( getDeploymentLogs
  , runDeployment
  ) where

import Import
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.HashMap as HashMap

import Deployments.Domain.Deployment
import qualified Kubernetes.Job as K8s
import qualified Kubernetes.Pod as K8s

getDeploymentLogs :: Maybe Int -> DeploymentId -> App (Maybe LBS.ByteString)
getDeploymentLogs nLines (DeploymentKey deploymentId) = do
  maybePod <- K8s.getPodForJob (uuidToText deploymentId)
  case maybePod of
    Nothing -> return Nothing
    Just pod@K8s.Pod {..} ->
      case K8s.getPodContainerState K8s.deploymentContainerName pod of
        Nothing -> return Nothing
        Just (K8s.Waiting _) -> return Nothing
        _ -> K8s.getLogs nLines podName

runDeployment :: Entity Deployment -> DeploymentResources -> App Bool
runDeployment (Entity deploymentId Deployment {..}) DeploymentResources {..} = K8s.createJob jobDescription
  where
    (Entity _ project) = deploymentProject
    (Entity _ environment) = deploymentEnvironment
    (Entity _ build) = deploymentBuild
    jobDescription =
      K8s.JobDescription
        { K8s.dockerImage = projectDeploymentImage project
        , K8s.envVars = environmentEnvVars environment <> buildParams build <> deployerParams
        , K8s.name = deploymentId
        }
    deployerParams = HashMap.fromList [("COOLOPS_DEPLOYER_NAME", fromMaybe "" deploymentUserName), ("COOLOPS_DEPLOYER_EXTERNAL_ID", deploymentDeployerExternalId)]
