module Handlers.MessageButtons.DeployBuild
  ( call
  ) where

import RIO
import qualified RIO.Text.Lazy as TL

import Web.Scotty.Trans

import Auth.Domain (CompanyID)
import qualified BackgroundJobs.AppJobs as Background
import Deployments.Classes
import qualified Deployments.Domain.Build as B
import Deployments.Domain.Deployment (DeploymentResources(..))
import qualified Deployments.Domain.Environment as E
import qualified Deployments.Domain.Project as P
import Slack.Api.IncomingWebhooks
import Slack.Api.Message
import Slack.Domain.Team (Team(..), webhookUrl)
import qualified Slack.UseCases.CreateDeployment as App
import qualified Slack.UseCases.NotifyNewBuild as NotifyBuild
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
      App.Params deploymentBuild deploymentEnvironment slackUserId slackUserName

getResources_ :: CompanyID -> Text -> Text -> WebHandler DeploymentResources
getResources_ cId eId bId = do
  maybeResources <- lift $ getDeploymentResources cId eId bId
  case maybeResources of
    Nothing -> text resourcesMissing >> finish
    Just resources -> return resources

notify :: (Text, Text) -> Team -> DeploymentResources -> WebHandler ()
notify (slackUserId, _) Team {..} DeploymentResources {..} =
  lift $ sendIncomingWebhook url message
  where
    url = webhookUrl teamIncomingWebhook
    bName = B.nameText $ B.buildName deploymentBuild
    eName = E.nameText $ E.environmentName deploymentEnvironment
    pName = P.nameText $ P.projectName deploymentProject
    message = slackMessage {messageText = Just t}
    t =
      "<@" <> slackUserId <> "> deployed *" <> bName <> "* to *" <> eName <>
      "* of *" <>
      pName <>
      "*"

call :: Team -> Text -> Text -> (Text, Text) -> WebHandler ()
call team@Team {..} environmentId buildId slackUser = do
  resources <- getResources_ teamCompanyId environmentId buildId
  runApp resources slackUser
  lift $ Background.notifyBuild teamCompanyId buildId
  -- notify slackUser team resources
