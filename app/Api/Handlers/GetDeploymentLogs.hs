module Handlers.GetDeploymentLogs
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Database.Queries.DeploymentForLogs
import Deployments.Gateway.Kubernetes
import Types (WebMonad)

getDeployment_ :: Text -> WebMonad Deployment
getDeployment_ deploymentId = do
  maybeDeployment <- lift $ getDeployment deploymentId
  case maybeDeployment of
    Nothing -> status notFound404 >> finish
    Just deployment -> return deployment

getDeploymentLogs_ :: Deployment -> WebMonad Text
getDeploymentLogs_ Deployment {..} = do
  maybeLogs <- lift $ getDeploymentLogs deploymentId
  case maybeLogs of
    Nothing -> return ""
    Just logs -> return $ decodeUtf8Lenient $ toStrictBytes logs

data Response = Response
  { resDeploymentFinished :: !Bool
  , resLogs :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} =
    object ["finished" .= resDeploymentFinished, "logs" .= resLogs]

respond :: Deployment -> Text -> WebMonad ()
respond Deployment {..} resLogs = json Response {..}
  where
    resDeploymentFinished = isFinished deploymentStatus

call :: WebMonad ()
call = do
  deploymentId <- param "id"
  deployment <- getDeployment_ deploymentId
  logs <- getDeploymentLogs_ deployment
  respond deployment logs
