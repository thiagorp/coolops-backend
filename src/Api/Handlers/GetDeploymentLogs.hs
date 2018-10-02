module Api.Handlers.GetDeploymentLogs
  ( getDeploymentLogsR
  ) where

import Api.Import

import Database.Queries.DeploymentForLogs
import Deployments.Gateway.Kubernetes

getDeployment_ :: Text -> Handler Deployment
getDeployment_ deploymentId = do
  maybeDeployment <- getDeployment deploymentId
  case maybeDeployment of
    Nothing -> sendResponseStatus notFound404 ()
    Just deployment -> return deployment

getDeploymentLogs_ :: Deployment -> Handler Text
getDeploymentLogs_ Deployment {..} = do
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

buildResponse :: Deployment -> Text -> Response
buildResponse Deployment {..} resLogs = Response {..}
  where
    resDeploymentFinished = isFinished deploymentStatus

getDeploymentLogsR :: Text -> Handler Value
getDeploymentLogsR deploymentId = do
  deployment <- getDeployment_ deploymentId
  logs <- getDeploymentLogs_ deployment
  return $ toJSON $ buildResponse deployment logs
