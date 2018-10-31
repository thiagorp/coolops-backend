module Slack.UseCases.CreateDeployment
  ( module Model
  , module Deployments.UseCases.CreateDeployment
  , CallMonad
  , Error(..)
  , Params(..)
  , call
  ) where

import RIO

import Data.Time

import qualified BackgroundJobs.AppJobs as Background
import qualified Deployments.UseCases.CreateDeployment as App (CallMonad, Error(..), Params(..), call)
import Deployments.UseCases.CreateDeployment hiding (CallMonad, Error(..), Params(..), call)
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

type CallMonad m = (MonadIO m, Background.NotifyBuildConstraint m)

createSlackDeployment_ :: (CallMonad m) => Params -> Entity Deployment -> Db m ()
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

call :: (App.CallMonad m, CallMonad m) => Params -> Db m (Either Error (Entity Deployment))
call params@Params {..} = do
  result <- lift $ App.call $ App.Params build environment
  case result of
    Left App.ProjectsDontMatch -> return $ Left ProjectsDontMatch
    Right d -> createSlackDeployment_ params d >> return (Right d)
