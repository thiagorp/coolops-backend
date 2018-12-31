module Slack.UseCases.CreateDeployment
  ( module Model
  , Error(..)
  , Params(..)
  , call
  ) where

import Import

import qualified BackgroundJobs.AppJobs as Background
import qualified Deployments.UseCases.CreateDeployment as App
import Model
import Slack.Database.BuildMessage (getSlackBuildMessage)


data Params = Params
  { build :: !(Entity Build)
  , environment :: !(Entity Environment)
  , companyId :: !CompanyId
  , slackUserId :: !Text
  , slackUserName :: !Text
  }


data Error =
  ProjectsDontMatch


createSlackDeployment_ :: Params -> Entity Deployment -> App ()
createSlackDeployment_ Params {..} deployment = do
  let (Entity buildId@(BuildKey buildKey) Build {..}) = build
      (Entity deploymentId Deployment {..}) = deployment
  maybeSlackBuildMessage <- getSlackBuildMessage buildId
  case maybeSlackBuildMessage of
    Nothing -> return ()
    Just (Entity slackBuildMessageId SlackBuildMessage {..}) -> do
      now <- liftIO getCurrentTime
      let slackDeploymentDeployedAt = now
      let slackDeploymentDeploymentId = deploymentId
      let slackDeploymentBuildMessageId = slackBuildMessageId
      let slackDeploymentCreatedAt = now
      let slackDeploymentUpdatedAt = now
      let slackDeploymentSlackUserId = slackUserId
      let slackDeploymentSlackUserName = slackUserName
      _ <- insert SlackDeployment {..}
      void $ Background.notifyBuild companyId buildKey


call :: Params -> App (Either Error (Entity Deployment))
call params@Params {..} = do
  result <- App.call $ App.Params build environment slackUserId
  case result of
    Left App.ProjectsDontMatch -> return $ Left ProjectsDontMatch
    Right d -> createSlackDeployment_ params d >> return (Right d)
