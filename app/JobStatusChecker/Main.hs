module Main where

import RIO

import Database.PostgreSQL.Simple
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Common.App
import Common.Config (PGSettings(..), pgSettings)
import Deployments.Database.Deployment as DB
import Deployments.Domain.Deployment hiding (run)
import qualified Deployments.UseCases.FinishDeployment as App
import qualified Kubernetes.Job as Job
import qualified Kubernetes.Pod as Pod
import Util.Key

data DeploymentJobStatus
  = StillRunning
  | Finished Status

deploymentPodStatus :: Job.Job -> AppT DeploymentJobStatus
deploymentPodStatus Job.Job {..} = do
  maybePod <- Pod.getPodForJob jobName
  case maybePod of
    Nothing -> return StillRunning
    Just pod ->
      case Pod.getPodContainerState Job.deploymentContainerName pod of
        Nothing -> return StillRunning
        Just Pod.Running -> return StillRunning
        Just Pod.Terminated -> return StillRunning
        Just (Pod.Waiting "ImagePullBackOff") ->
          return $ Finished (Failed InvalidDockerImage)
        Just (Pod.Waiting _) -> return StillRunning

deploymentJobStatus :: RunningDeployment -> AppT DeploymentJobStatus
deploymentJobStatus RunningDeployment {..} = do
  maybeJob <- Job.getJob $ keyByteString runningDeploymentId
  case maybeJob of
    Nothing -> return $ Finished (Failed JobNotFound)
    Just job ->
      case Job.readStatus job of
        Job.Running -> deploymentPodStatus job
        Job.Failed -> return $ Finished (Failed JobFailed)
        Job.Succeeded -> return $ Finished Succeeded

syncJobStatus :: (CompanyID, RunningDeployment) -> AppT ()
syncJobStatus (companyId, deployment) = do
  maybeStatus <- deploymentJobStatus deployment
  case maybeStatus of
    StillRunning -> return ()
    Finished status -> void $ App.call status companyId deployment

app :: AppT ()
app = do
  deployments <- DB.listAllRunningDeployments
  mapM_ syncJobStatus deployments

loopWith :: Env -> IO ()
loopWith env = do
  run app env
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  conn <- pgSettings >>= connectPostgreSQL . pgUrl
  requestManager <-
    newTlsManagerWith
      (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv conn requestManager
  loopWith env
