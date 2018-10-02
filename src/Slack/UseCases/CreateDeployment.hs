module Slack.UseCases.CreateDeployment where

import RIO

import Data.Time

import qualified BackgroundJobs.AppJobs as Background
import Common.Database (HasPostgres)
import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Deployment as Deployment
import qualified Deployments.Domain.Environment as Environment
import qualified Deployments.Domain.Project as Project
import qualified Deployments.UseCases.CreateDeployment as App
import Slack.Database.BuildMessage (getSlackBuildMessage)
import Slack.Database.Deployment (createSlackDeployment)
import Slack.Domain.BuildMessage hiding (genId)
import Slack.Domain.Deployment
import Util.Key

data Params = Params
  { build :: !Build.Build
  , environment :: !Environment.Environment
  , companyId :: !Project.CompanyID
  , deploymentSlackUserId :: !Text
  , deploymentSlackUserName :: !Text
  }

data Error =
  ProjectsDontMatch

type CallMonad m = (HasPostgres m, Background.NotifyBuildConstraint m)

createSlackDeployment_ :: CallMonad m => Params -> Deployment.QueuedDeployment -> m ()
createSlackDeployment_ Params {..} queuedDeployment = do
  maybeSlackBuildMessage <- getSlackBuildMessage (Build.buildId build)
  case maybeSlackBuildMessage of
    Nothing -> return ()
    Just BuildMessage {..} -> do
      deploymentId <- genId
      deploymentDeployedAt <- liftIO getCurrentTime
      let deploymentDeploymentId = Deployment.deploymentId queuedDeployment
      let deploymentBuildMessageId = buildMessageId
      createSlackDeployment Deployment {..}
      Background.notifyBuild companyId (keyText $ Build.buildId build)

call :: (App.CallMonad m, CallMonad m) => Params -> m (Either Error Deployment.QueuedDeployment)
call params@Params {..} = do
  result <- App.call $ App.Params build environment
  case result of
    Left App.ProjectsDontMatch -> return $ Left ProjectsDontMatch
    Right d -> createSlackDeployment_ params d >> return (Right d)
