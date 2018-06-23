module Main where

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.Text as Text

import Database.PostgreSQL.Simple
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Common.App hiding (kubernetesSettings)
import Common.Config (PGSettings(..), kubernetesSettings, pgSettings)
import Deployments.Database.Deployment as DB
import Deployments.Domain.Deployment hiding (run)
import qualified Kubernetes.Job as Job
import qualified Kubernetes.Pod as Pod
import Util.Key

deploymentPodStatus :: Job.Job -> AppT (Maybe Status)
deploymentPodStatus Job.Job {..} = do
  maybePod <- Pod.getPodForJob $ Text.encodeUtf8 jobName
  case maybePod of
    Nothing -> return Nothing
    Just pod ->
      case Pod.getPodContainerState Job.deploymentContainerName pod of
        Nothing -> return Nothing
        Just Pod.Running -> return Nothing
        Just Pod.Terminated -> return Nothing
        Just (Pod.Waiting "ImagePullBackOff") ->
          return $ Just (Failed InvalidDockerImage)
        Just (Pod.Waiting _) -> return Nothing

deploymentJobStatus :: RunningDeployment -> AppT (Maybe Status)
deploymentJobStatus RunningDeployment {..} = do
  maybeJob <- Job.getJob $ keyByteString runningDeploymentId
  case maybeJob of
    Nothing -> return $ Just (Unknown JobNotFound)
    Just job ->
      case Job.readStatus job of
        Job.Running -> deploymentPodStatus job
        Job.Failed -> return $ Just (Failed JobFailed)
        Job.Succeeded -> return $ Just Succeeded

syncJobStatus :: RunningDeployment -> AppT ()
syncJobStatus deployment = do
  BS.putStr
    ("Checking status of deployment " <>
     keyByteString (runningDeploymentId deployment) <>
     " --- ")
  maybeStatus <- deploymentJobStatus deployment
  case maybeStatus of
    Nothing -> BS.putStr "Still running\n"
    Just (Unknown JobNotFound) -> BS.putStr "Job not found\n"
    Just (Failed JobFailed) -> BS.putStr "Deployment failed\n"
    Just (Failed InvalidDockerImage) -> BS.putStr "Invalid docker image\n"
    Just (Succeeded) -> BS.putStr "Deployment succeeded\n"

app :: AppT ()
app = do
  deployments <- DB.listAllRunningDeployments
  _ <- mapM syncJobStatus deployments
  return ()

loopWith :: Env -> IO ()
loopWith env = do
  run app env
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  conn <- pgSettings >>= connectPostgreSQL . pgUrl
  k8sSettings <- kubernetesSettings
  requestManager <-
    newTlsManagerWith
      (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  loopWith (Env conn requestManager k8sSettings)
