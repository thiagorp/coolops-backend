module Handlers.MessageButtons.DeployBuild
  ( call
  ) where

import RIO
import qualified RIO.Text.Lazy as TL

import Web.Scotty.Trans

import Auth.Domain (CompanyID)
import Deployments.Classes
import Deployments.Domain.Deployment (DeploymentResources(..))
import qualified Deployments.Domain.Project as P
import Slack.Domain.Team (Team(..))
import qualified Slack.UseCases.CreateDeployment as App
import Types

resourcesMissing :: TL.Text
resourcesMissing =
  "Either the build or the environment that you are trying to deploy does not exist anymore"

projectsDontMatch :: TL.Text
projectsDontMatch =
  "There was a very unexpected error. We are already notified and are working on solving it as soon as possible"

runApp :: DeploymentResources -> (Text, Text) -> WebHandler ()
runApp DeploymentResources {..} (slackUserId, slackUserName) = do
  result <- lift $ App.call appParams
  case result of
    Right _ -> return ()
    Left App.ProjectsDontMatch -> text projectsDontMatch >> finish
  where
    appParams =
      App.Params
        deploymentBuild
        deploymentEnvironment
        (P.projectCompanyId deploymentProject)
        slackUserId
        slackUserName

getResources_ :: CompanyID -> Text -> Text -> WebHandler DeploymentResources
getResources_ cId eId bId = do
  maybeResources <- lift $ getDeploymentResources cId eId bId
  case maybeResources of
    Nothing -> text resourcesMissing >> finish
    Just resources -> return resources

call :: Team -> Text -> Text -> (Text, Text) -> WebHandler ()
call Team {..} environmentId buildId slackUser = do
  resources <- getResources_ teamCompanyId environmentId buildId
  runApp resources slackUser
