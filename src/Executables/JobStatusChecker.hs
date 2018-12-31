{-# LANGUAGE NoImplicitPrelude #-}

module Executables.JobStatusChecker (run) where

import Import

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Deployments.Database.Deployment as DB
import qualified Deployments.UseCases.FinishDeployment as App
import qualified Kubernetes.Job as Job
import qualified Kubernetes.Pod as Pod

data DeploymentJobStatus
  = StillRunning
  | Finished DeploymentStatus

deploymentPodStatus :: Job.Job -> App DeploymentJobStatus
deploymentPodStatus Job.Job {..} = do
  maybePod <- Pod.getPodForJob jobName
  case maybePod of
    Nothing -> return StillRunning
    Just pod ->
      case Pod.getPodContainerState Job.deploymentContainerName pod of
        Nothing -> return StillRunning
        Just Pod.Running -> return StillRunning
        Just Pod.Terminated -> return StillRunning
        Just (Pod.Waiting "ImagePullBackOff") -> return $ Finished (Failed "invalid_docker_image")
        Just (Pod.Waiting _) -> return StillRunning

deploymentJobStatus :: Entity Deployment -> App DeploymentJobStatus
deploymentJobStatus (Entity (DeploymentKey deploymentKey) Deployment {..}) = do
  maybeJob <- Job.getJob $ uuidToText deploymentKey
  case maybeJob of
    Nothing -> return $ Finished (Failed "job_not_found")
    Just job ->
      case Job.readStatus job of
        Job.Running -> deploymentPodStatus job
        Job.Failed -> return $ Finished (Failed "job_failed")
        Job.Succeeded -> return $ Finished Succeeded

syncJobStatus :: (CompanyId, Entity Deployment) -> App ()
syncJobStatus (companyId, deployment) = do
  maybeStatus <- deploymentJobStatus deployment
  case maybeStatus of
    StillRunning -> return ()
    Finished status -> void $ App.call status companyId deployment

app :: App ()
app = do
  deployments <- DB.listAllRunningDeployments
  mapM_ syncJobStatus deployments

loopWith :: Env -> IO ()
loopWith env = do
  runApp env app
  threadDelay 1000000
  loopWith env

run :: IO ()
run = do
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  loopWith env
