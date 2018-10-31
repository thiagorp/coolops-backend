module Main where

import RIO

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Deployments.Database.Deployment as DB
import qualified Deployments.UseCases.FinishDeployment as App
import Env
import qualified Kubernetes.Job as Job
import qualified Kubernetes.Pod as Pod

type AppT = RIO Env

data DeploymentJobStatus
  = StillRunning
  | Finished DeploymentStatus

deploymentPodStatus :: Job.Job -> Db AppT DeploymentJobStatus
deploymentPodStatus Job.Job {..} = do
  maybePod <- lift $ Pod.getPodForJob jobName
  case maybePod of
    Nothing -> return StillRunning
    Just pod ->
      case Pod.getPodContainerState Job.deploymentContainerName pod of
        Nothing -> return StillRunning
        Just Pod.Running -> return StillRunning
        Just Pod.Terminated -> return StillRunning
        Just (Pod.Waiting "ImagePullBackOff") -> return $ Finished (Failed "invalid_docker_image")
        Just (Pod.Waiting _) -> return StillRunning

deploymentJobStatus :: Entity Deployment -> Db AppT DeploymentJobStatus
deploymentJobStatus (Entity (DeploymentKey deploymentKey) Deployment {..}) = do
  maybeJob <- lift $ Job.getJob $ uuidToByteString deploymentKey
  case maybeJob of
    Nothing -> return $ Finished (Failed "job_not_found")
    Just job ->
      case Job.readStatus job of
        Job.Running -> deploymentPodStatus job
        Job.Failed -> return $ Finished (Failed "job_failed")
        Job.Succeeded -> return $ Finished Succeeded

syncJobStatus :: (CompanyId, Entity Deployment) -> Db AppT ()
syncJobStatus (companyId, deployment) = do
  maybeStatus <- deploymentJobStatus deployment
  case maybeStatus of
    StillRunning -> return ()
    Finished status -> void $ App.call status companyId deployment

app :: AppT ()
app = do
  deployments <- runDb DB.listAllRunningDeployments
  mapM_ (runDb . syncJobStatus) deployments

loopWith :: Env -> IO ()
loopWith env = do
  runRIO env app
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  loopWith env
