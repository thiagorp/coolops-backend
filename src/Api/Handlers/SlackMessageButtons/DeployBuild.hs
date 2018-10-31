module Api.Handlers.SlackMessageButtons.DeployBuild
  ( call
  ) where

import Api.Import

import qualified RIO.Text.Lazy as TL

import Deployments.Database.Deployment (getDeploymentResources)
import Deployments.Domain.Deployment (DeploymentResources(..))
import qualified Slack.Database.AccessToken as AT
import Slack.UseCases.CreateDeployment hiding (call)
import qualified Slack.UseCases.CreateDeployment as App (call)

resourcesMissing :: TL.Text
resourcesMissing = "Either the build or the environment that you are trying to deploy does not exist anymore"

projectsDontMatch :: TL.Text
projectsDontMatch =
  "There was a very unexpected error. We are already notified and are working on solving it as soon as possible"

integrationMissing :: TL.Text
integrationMissing = "Something wrong happened and we can't find your Slack integration"

runApp :: DeploymentResources -> (Text, Text) -> Db Handler ()
runApp DeploymentResources {..} (slackUserId, slackUserName) = do
  result <- App.call appParams
  case result of
    Right _ -> return ()
    Left ProjectsDontMatch -> sendResponse projectsDontMatch
  where
    Entity _ project = deploymentProject
    appParams = Params deploymentBuild deploymentEnvironment (projectCompanyId project) slackUserId slackUserName

getDeploymentResources_ :: UUID -> UUID -> CompanyId -> Db Handler (Maybe DeploymentResources)
getDeploymentResources_ eId bId cId = getDeploymentResources cId eId bId

getResources_ :: [CompanyId] -> UUID -> UUID -> Db Handler DeploymentResources
getResources_ cId eId bId = do
  maybeResources <- traverse (getDeploymentResources_ eId bId) cId
  case catMaybes maybeResources of
    [] -> sendResponse resourcesMissing
    (resources:_) -> return resources

getCompanyId_ :: Text -> Db Handler [CompanyId]
getCompanyId_ teamId = do
  accessTokens <- AT.findByTeamId teamId
  case accessTokens of
    [] -> sendResponse integrationMissing
    _ -> return (map (slackAccessTokenCompanyId . entityVal) accessTokens)

readUUID_ :: Text -> Handler UUID
readUUID_ t =
  case textToUUID t of
    Nothing -> invalidArgs []
    Just u -> return u

call :: Text -> Text -> Text -> (Text, Text) -> Handler ()
call environmentTextId buildTextId teamId slackUser = do
  environmentId <- readUUID_ environmentTextId
  buildId <- readUUID_ buildTextId
  runDb $ do
    companyId <- getCompanyId_ teamId
    resources <- getResources_ companyId environmentId buildId
    runApp resources slackUser
