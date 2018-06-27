module Handlers.MessageButtons.DeployBuild
  ( call
  ) where

import RIO
import qualified RIO.Text.Lazy as TL

import Web.Scotty.Trans

import Auth.Domain (CompanyID)
import Common.App (AppT)
import Deployments.Classes
import qualified Deployments.Domain.Build as B
import qualified Deployments.Domain.Environment as E
import qualified Deployments.Domain.Project as P
import qualified Deployments.UseCases.CreateDeployment as App
import Slack.Api.IncomingWebhooks
import Slack.Api.Message
import Slack.Domain.Team (Team(..), webhookUrl)
import Types
import Util.Key

runApp :: B.Build -> E.Environment -> WebHandler ()
runApp build environment =
  lift $ App.call (App.Params build environment) >> return ()

withError :: TL.Text -> AppT (Maybe a) -> WebHandler a
withError errorText v = do
  maybeVal <- lift v
  case maybeVal of
    Nothing -> text errorText >> finish
    Just val -> return val

buildMissing :: TL.Text
buildMissing = "_This build doesn't exist anymore_"

environmentMissing :: TL.Text
environmentMissing = "_This environment doesn't exist anymore_"

projectMissing :: TL.Text
projectMissing = "_This project doesn't exist anymore_"

fetchEnvironment :: CompanyID -> Text -> WebHandler E.Environment
fetchEnvironment cId eId = withError environmentMissing $ getEnvironment cId eId

fetchBuild :: CompanyID -> Text -> WebHandler B.Build
fetchBuild cId eId = withError buildMissing $ getBuild cId eId

fetchProject :: CompanyID -> Text -> WebHandler P.Project
fetchProject cId pId = withError projectMissing $ getProject cId pId

notify :: Text -> Team -> B.Build -> E.Environment -> P.Project -> WebHandler ()
notify senderId (Team {..}) (B.Build {..}) (E.Environment {..}) (P.Project {..}) =
  lift $ sendIncomingWebhook url message
  where
    url = webhookUrl teamIncomingWebhook
    message = slackMessage {messageText = Just t}
    t =
      "<@" <> senderId <> "> deployed *" <> B.nameText buildName <> "* to *" <>
      E.nameText environmentName <>
      "* of *" <>
      P.nameText projectName <>
      "*"

call :: Team -> Text -> Text -> Text -> WebHandler ()
call (team@Team {..}) environmentId buildId reqSender = do
  build <- fetchBuild teamCompanyId buildId
  environment <- fetchEnvironment teamCompanyId environmentId
  project <- fetchProject teamCompanyId $ keyText $ B.buildProjectId build
  runApp build environment
  notify reqSender team build environment project
