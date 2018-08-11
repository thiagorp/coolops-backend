module Handlers.GetSlackProjectIntegration
  ( call
  ) where

import RIO

import Network.HTTP.Types.Status (notFound404)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Classes (getProject)
import Deployments.Domain.Project (Project(..))
import Resources
import Slack.Api.Classes (slackClientId)
import Slack.Database.ProjectIntegration (getSlackIntegrationForProject)
import Types (WebMonad)

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  pId <- param "project_id"
  maybeProject <- lift $ getProject (userCompanyId user) pId
  clientId <- lift slackClientId
  case maybeProject of
    Nothing -> status notFound404
    Just Project {..} -> do
      maybeSlackIntegration <- lift $ getSlackIntegrationForProject projectId
      json $ slackProjectIntegrationResource clientId maybeSlackIntegration
