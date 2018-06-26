module Slack.UseCases.NotifyNewBuild
  ( CallConstraint
  , call
  ) where

import RIO

import Deployments.Classes
import qualified Deployments.Domain.Build as B
import qualified Deployments.Domain.Environment as E
import qualified Deployments.Domain.Project as P
import Slack.Api.IncomingWebhooks
import Slack.Api.Message
import Slack.Classes
import Slack.Domain.Team
import Util.Key

data Error =
  ProjectDoesNotexist

sendMessage ::
     (SlackClientMonad m)
  => Team
  -> B.Build
  -> P.Project
  -> [E.Environment]
  -> m ()
sendMessage Team {..} B.Build {..} P.Project {..} environments = do
  sendIncomingWebhook (webhookUrl teamIncomingWebhook) message
  where
    buildAction E.Environment {..} =
      slackAction
        { actionName = "environment"
        , actionText = "Deploy to " <> E.nameText environmentName
        , actionType = "button"
        , actionValue = keyText environmentId
        }
    messageText =
      "*" <> P.nameText projectName <> "* has a new build: *" <>
      B.nameText buildName <>
      "*"
    attachments =
      slackAttachment
        { attachmentPretext = Just messageText
        , attachmentCallbackId = Just $ "deploy_build|" <> keyText buildId
        , attachmentType = Just "default"
        , attachmentActions = Just $ map buildAction environments
        }
    message = slackMessage {messageAttachments = Just [attachments]}

type CallConstraint m
   = (ProjectRepo m, EnvironmentRepo m, SlackTeamRepo m, SlackClientMonad m)

call :: CallConstraint m => B.Build -> m (Either Error Bool)
call build = do
  maybeProject <- getProjectForBuild build
  case maybeProject of
    Nothing -> return $ Left ProjectDoesNotexist
    Just project -> do
      maybeSlackTeam <- getSlackTeam $ P.projectCompanyId project
      case maybeSlackTeam of
        Nothing -> return $ Right False
        Just slackTeam -> do
          environments <- listEnvironments $ P.projectCompanyId project
          sendMessage slackTeam build project environments
          return $ Right True
