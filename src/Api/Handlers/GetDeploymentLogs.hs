module Api.Handlers.GetDeploymentLogs
  ( getDeploymentLogsR
  ) where

import Api.Import

import Deployments.Database.Deployment
import Deployments.Gateway.Kubernetes

getDeployment_ :: UUID -> Handler (Entity Deployment)
getDeployment_ deploymentId = do
  maybeDeployment <- runDb $ getDeployment deploymentId
  case maybeDeployment of
    Nothing -> sendResponseStatus notFound404 ()
    Just deployment -> return deployment

getDeploymentLogs_ :: Entity Deployment -> Handler Text
getDeploymentLogs_ (Entity deploymentId _) = do
  maybeLogs <- getDeploymentLogs Nothing deploymentId
  case maybeLogs of
    Nothing -> return ""
    Just logs -> return $ decodeUtf8Lenient $ toStrictBytes logs

data Response = Response
  { resDeploymentFinished :: !Bool
  , resLogs :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["finished" .= resDeploymentFinished, "logs" .= resLogs]

buildResponse :: Entity Deployment -> Text -> Response
buildResponse (Entity _ Deployment {..}) resLogs = Response {..}
  where
    resDeploymentFinished = isFinished deploymentStatus

getDeploymentLogsR :: UUID -> Handler Value
getDeploymentLogsR deploymentId = do
  deployment <- getDeployment_ deploymentId
  logs <- getDeploymentLogs_ deployment
  return $ toJSON $ buildResponse deployment logs
