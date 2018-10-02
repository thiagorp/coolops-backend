module Api.Handlers.SlackMessageButtons.DeployBuild
  ( call
  ) where

import Api.Import

import qualified RIO.Text.Lazy as TL

import Auth.Domain (CompanyID)
import Deployments.Database.Deployment (getDeploymentResources)
import Deployments.Domain.Deployment (DeploymentResources(..))
import qualified Deployments.Domain.Project as P
import Slack.Database.ProjectIntegration (getCompanyIdForBuildsProjectIntegration)
import qualified Slack.UseCases.CreateDeployment as App

resourcesMissing :: TL.Text
resourcesMissing = "Either the build or the environment that you are trying to deploy does not exist anymore"

projectsDontMatch :: TL.Text
projectsDontMatch =
  "There was a very unexpected error. We are already notified and are working on solving it as soon as possible"

integrationMissing :: TL.Text
integrationMissing = "Something wrong happened and we can't find your Slack integration"

runApp :: DeploymentResources -> (Text, Text) -> Handler ()
runApp DeploymentResources {..} (slackUserId, slackUserName) = do
  result <- App.call appParams
  case result of
    Right _ -> return ()
    Left App.ProjectsDontMatch -> sendResponse projectsDontMatch
  where
    appParams =
      App.Params deploymentBuild deploymentEnvironment (P.projectCompanyId deploymentProject) slackUserId slackUserName

getResources_ :: CompanyID -> Text -> Text -> Handler DeploymentResources
getResources_ cId eId bId = do
  maybeResources <- getDeploymentResources cId eId bId
  case maybeResources of
    Nothing -> sendResponse resourcesMissing
    Just resources -> return resources

getCompanyId_ :: Text -> Text -> Handler CompanyID
getCompanyId_ teamId buildId = do
  maybeCompanyId <- getCompanyIdForBuildsProjectIntegration teamId buildId
  case maybeCompanyId of
    Nothing -> sendResponse integrationMissing
    Just companyId -> return companyId

call :: Text -> Text -> Text -> (Text, Text) -> Handler ()
call environmentId buildId teamId slackUser = do
  companyId <- getCompanyId_ teamId buildId
  resources <- getResources_ companyId environmentId buildId
  runApp resources slackUser
