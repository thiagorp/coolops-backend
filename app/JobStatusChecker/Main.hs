module Main where

import RIO
import qualified RIO.Text as Text

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
    Nothing -> return $ Just (Failed JobNotFound)
    Just job ->
      case Job.readStatus job of
        Job.Running -> deploymentPodStatus job
        Job.Failed -> return $ Just (Failed JobFailed)
        Job.Succeeded -> return $ Just Succeeded

syncJobStatus :: (CompanyID, RunningDeployment) -> AppT ()
syncJobStatus (companyId, deployment) = do
  maybeStatus <- deploymentJobStatus deployment
  case maybeStatus of
    Nothing -> return ()
    Just status -> void $ App.call status companyId deployment

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
