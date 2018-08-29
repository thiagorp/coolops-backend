{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.GraphQL where

import RIO hiding (Enum, Handler)
import qualified RIO.Map as Map

import Data.Aeson hiding (Object, json)
import Web.Scotty.Trans hiding (Param)

import GraphQL
import GraphQL.API
import GraphQL.Resolver ((:<>)(..), Handler, HasResolver(..))

import qualified Authorization as Auth
import Common.App (Env(..))
import qualified Common.Config as Config
import qualified GraphQL.Database as DB
import qualified GraphQL.Database.Types as DB
import Types (WebMonad)
import Util.Key as Key

type App = DB.App

type Project
   = Object "Project" '[] '[ Field "id" Text, Field "name" Text, Field "deploymentImage" Text, Field "accessToken" Text, Field "environments" (List Environment), Field "slackIntegration" (Maybe SlackProjectIntegration), Field "createdAt" Int32, Field "updatedAt" Int32]

newtype EnvironmentProjectD m =
  EnvironmentProjectD (Handler m Project)

data EnvironmentProject =
  EnvironmentProject

instance forall m. (Monad m) => HasResolver m EnvironmentProject where
  type Handler m EnvironmentProject = EnvironmentProjectD m
  resolve (EnvironmentProjectD rd) = resolve @m @Project rd

instance HasAnnotatedType EnvironmentProject where
  getAnnotatedType = getAnnotatedType @Int

type Environment
   = Object "Environment" '[] '[ Field "id" Text, Field "name" Text, Field "environmentVariables" (List Param), Field "lastDeployment" (Maybe Deployment), Field "project" EnvironmentProject, Field "createdAt" Int32, Field "updatedAt" Int32]

type Build
   = Object "Build" '[] '[ Field "id" Text, Field "name" Text, Field "project" Project, Field "params" (List Param), Field "metadata" (List Param), Field "createdAt" Int32, Field "updatedAt" Int32]

type Company
   = Object "Company" '[] '[ Field "id" Text, Field "name" Text, Field "createdAt" Int32, Field "updatedAt" Int32]

newtype DeploymentBuildD m =
  DeploymentBuildD (Handler m Build)

data DeploymentBuild =
  DeploymentBuild

instance forall m. (Monad m) => HasResolver m DeploymentBuild where
  type Handler m DeploymentBuild = DeploymentBuildD m
  resolve (DeploymentBuildD rd) = resolve @m @Build rd

instance HasAnnotatedType DeploymentBuild where
  getAnnotatedType = getAnnotatedType @Int

type Deployment
   = Object "Deployment" '[] '[ Field "id" Text, Field "startedAt" (Maybe Int32), Field "status" (Enum "DeploymentStatus" DB.DeploymentStatus), Field "build" DeploymentBuild, Field "createdAt" Int32, Field "updatedAt" Int32]

type Param = Object "Param" '[] '[ Field "key" Text, Field "value" Text]

type SlackConfiguration
   = Object "SlackConfiguration" '[] '[ Field "clientId" Text]

type SlackProjectIntegration
   = Object "SlackProjectIntegration" '[] '[ Field "workspaceName" Text]

type User
   = Object "User" '[] '[ Field "id" Text, Field "firstName" Text, Field "lastName" Text, Field "email" Text, Field "company" Company, Field "createdAt" Int32, Field "updatedAt" Int32]

type Query
   = Object "Query" '[] '[ Argument "id" Text :> Field "environment" (Maybe Environment), Field "projects" (List Project), Argument "page" (Maybe Int32) :> Argument "pageSize" (Maybe Int32) :> Field "builds" (List Build), Argument "id" Text :> Field "project" (Maybe Project), Field "slackConfiguration" SlackConfiguration, Field "me" User]

-- Datasource
getBuild_ :: DB.BuildID -> Handler App Build
getBuild_ id_ = do
  maybeBuild <- DB.getBuild id_
  case maybeBuild of
    Just b -> buildHandler b
    Nothing -> fail "Build not found"

getCompany_ :: Handler App Company
getCompany_ = do
  maybe_ <- DB.getCompany
  case maybe_ of
    Just c -> companyHandler c
    Nothing -> fail "Company not found"

getEnvironment :: DB.EnvironmentID -> Handler App (Maybe Environment)
getEnvironment id_ = do
  maybe_ <- DB.getEnvironment id_
  case maybe_ of
    Just e -> pure $ Just (environmentHandler e)
    Nothing -> return Nothing

getProject_ :: DB.ProjectID -> Handler App Project
getProject_ pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> projectHandler p
    Nothing -> fail "Project not found"

getProject :: DB.ProjectID -> Handler App (Maybe Project)
getProject pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> pure $ Just (projectHandler p)
    Nothing -> return Nothing

getEnvLastDeployment :: DB.EnvironmentID -> Handler App (Maybe Deployment)
getEnvLastDeployment eId = do
  maybeDeployment <- DB.getEnvLastDeployment eId
  case maybeDeployment of
    Just d -> pure $ Just (deploymentHandler d)
    Nothing -> return Nothing

getSlackConfiguration :: Config.SlackSettings -> Handler App SlackConfiguration
getSlackConfiguration Config.SlackSettings {..} = pure $ pure slackClientId

getSlackProjectIntegration ::
     DB.ProjectID -> Handler App (Maybe SlackProjectIntegration)
getSlackProjectIntegration pId = do
  maybe_ <- DB.getSlackProjectIntegration pId
  case maybe_ of
    Just e -> pure $ Just (slackProjectIntegrationHandler e)
    Nothing -> return Nothing

getUser_ :: DB.UserID -> Handler App User
getUser_ pId = do
  maybe_ <- DB.getUser pId
  case maybe_ of
    Just e -> userHandler e
    Nothing -> fail "User not found"

listBuilds :: Maybe Int32 -> Maybe Int32 -> Handler App (List Build)
listBuilds page pageSize =
  map buildHandler <$>
  DB.listBuilds
    (fromIntegral $ fromMaybe 1 page, fromIntegral $ fromMaybe 20 pageSize)

listEnvironments :: DB.ProjectID -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

-- Handlers
companyHandler :: DB.Company -> Handler App Company
companyHandler DB.Company {..} =
  pure $
  pure (DB.idText companyId) :<> pure companyName :<> pure companyCreatedAt :<>
  pure companyUpdatedAt

deploymentHandler :: DB.Deployment -> Handler App Deployment
deploymentHandler DB.Deployment {..} =
  pure $
  pure (DB.idText deploymentId) :<> pure (pure <$> deploymentStartedAt) :<>
  pure deploymentStatus :<>
  (DeploymentBuildD $ getBuild_ deploymentBuildId) :<>
  pure deploymentCreatedAt :<>
  pure deploymentUpdatedAt

environmentHandler :: DB.Environment -> Handler App Environment
environmentHandler DB.Environment {..} =
  pure $
  pure (DB.idText envId) :<> pure envName :<> pure (map paramHandler envEnvVars) :<>
  getEnvLastDeployment envId :<>
  (EnvironmentProjectD $ getProject_ envProjectId) :<>
  pure envCreatedAt :<>
  pure envUpdatedAt

paramHandler :: (Text, Text) -> Handler App Param
paramHandler (key, value) = pure $ pure key :<> pure value

projectHandler :: DB.Project -> Handler App Project
projectHandler DB.Project {..} =
  pure $
  pure (DB.idText projectId) :<> pure projectName :<>
  pure projectDeploymentImage :<>
  pure projectAccessToken :<>
  listEnvironments projectId :<>
  getSlackProjectIntegration projectId :<>
  pure projectCreatedAt :<>
  pure projectUpdatedAt

buildHandler :: DB.Build -> Handler App Build
buildHandler DB.Build {..} =
  pure $
  pure (DB.idText buildId) :<> pure buildName :<> getProject_ buildProjectId :<>
  pure (map paramHandler buildParams) :<>
  pure (map paramHandler buildMetadata) :<>
  pure buildCreatedAt :<>
  pure buildUpdatedAt

slackProjectIntegrationHandler ::
     DB.SlackProjectIntegration -> Handler App SlackProjectIntegration
slackProjectIntegrationHandler DB.SlackProjectIntegration {..} =
  pure $ pure spiWorkspaceName

userHandler :: DB.User -> Handler App User
userHandler DB.User {..} =
  pure $
  pure (DB.idText userId) :<> pure userFirstName :<> pure userLastName :<>
  pure userEmail :<>
  getCompany_ :<>
  pure userCreatedAt :<>
  pure userUpdatedAt

handler :: Auth.User -> Env -> Handler App Query
handler Auth.User {..} Env {..} =
  pure $
  (getEnvironment . DB.ID) :<> listProjects :<> listBuilds :<>
  (getProject . DB.ID) :<>
  getSlackConfiguration slackSettings :<>
  getUser_ (DB.ID (Key.keyText userId))

newtype Request = Request
  { reqQuery :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request" $ \o -> do
      reqQuery <- o .: "query"
      return Request {..}

call :: Auth.AuthenticatedUser -> WebMonad ()
call (Auth.AuthenticatedUser user) = do
  Request {..} <- jsonData
  appEnv <- lift ask
  env <- lift $ DB.buildEnv user appEnv
  result <-
    lift $
    DB.run
      env
      (interpretQuery @Query (handler user appEnv) reqQuery Nothing Map.empty)
  json result
