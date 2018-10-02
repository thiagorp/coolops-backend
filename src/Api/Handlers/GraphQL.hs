{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Api.Handlers.GraphQL
  ( postGraphQLR
  ) where

import Api.Import hiding (Enum, Handler, Object, Project, User)
import qualified Api.Import as Api

import qualified RIO.Map as Map

import GraphQL
import GraphQL.API
import GraphQL.Resolver ((:<>)(..), Handler, HasResolver(..))

import qualified Common.Config as Config
import Env
import qualified GraphQL.Database as DB
import qualified GraphQL.Database.Types as DB
import Util.Key as Key

type App = DB.App

newtype ProjectBuildD m =
  ProjectBuildD (Handler m Build)

data ProjectBuild

instance forall m. (Monad m) => HasResolver m ProjectBuild where
  type Handler m ProjectBuild = ProjectBuildD m
  resolve (ProjectBuildD rd) = resolve @m @Build rd

instance HasAnnotatedType ProjectBuild where
  getAnnotatedType = getAnnotatedType @Int

type Project
   = Object "Project" '[] '[ Field "id" Text, Field "name" Text, Field "slug" Text, Field "deploymentImage" Text, Field "accessToken" Text, Field "environments" (List Environment), Argument "page" (Maybe Int32) :> Argument "pageSize" (Maybe Int32) :> Field "builds" (List ProjectBuild), Field "slackIntegration" (Maybe SlackProjectIntegration), Field "createdAt" Int32, Field "updatedAt" Int32]

newtype EnvironmentProjectD m =
  EnvironmentProjectD (Handler m Project)

data EnvironmentProject

instance forall m. (Monad m) => HasResolver m EnvironmentProject where
  type Handler m EnvironmentProject = EnvironmentProjectD m
  resolve (EnvironmentProjectD rd) = resolve @m @Project rd

instance HasAnnotatedType EnvironmentProject where
  getAnnotatedType = getAnnotatedType @Int

type Environment
   = Object "Environment" '[] '[ Field "id" Text, Field "name" Text, Field "slug" Text, Field "environmentVariables" (List Param), Field "lastDeployment" (Maybe Deployment), Field "project" EnvironmentProject, Field "createdAt" Int32, Field "updatedAt" Int32]

type Build
   = Object "Build" '[] '[ Field "id" Text, Field "name" Text, Field "project" Project, Field "params" (List Param), Field "metadata" (List Param), Field "createdAt" Int32, Field "updatedAt" Int32]

type Company
   = Object "Company" '[] '[ Field "id" Text, Field "name" Text, Field "onboardingCompleted" Bool, Field "createdAt" Int32, Field "updatedAt" Int32]

newtype DeploymentBuildD m =
  DeploymentBuildD (Handler m Build)

data DeploymentBuild

instance forall m. (Monad m) => HasResolver m DeploymentBuild where
  type Handler m DeploymentBuild = DeploymentBuildD m
  resolve (DeploymentBuildD rd) = resolve @m @Build rd

instance HasAnnotatedType DeploymentBuild where
  getAnnotatedType = getAnnotatedType @Int

type Deployment
   = Object "Deployment" '[] '[ Field "id" Text, Field "startedAt" (Maybe Int32), Field "status" (Enum "DeploymentStatus" DB.DeploymentStatus), Field "build" DeploymentBuild, Field "createdAt" Int32, Field "updatedAt" Int32]

type Param = Object "Param" '[] '[ Field "key" Text, Field "value" Text]

type SlackConfiguration = Object "SlackConfiguration" '[] '[ Field "clientId" Text]

type SlackProjectIntegration = Object "SlackProjectIntegration" '[] '[ Field "workspaceName" Text]

type User
   = Object "User" '[] '[ Field "id" Text, Field "firstName" Text, Field "lastName" Text, Field "email" Text, Field "company" Company, Field "createdAt" Int32, Field "updatedAt" Int32]

type Onboarding = Object "Onboarding" '[] '[ Field "company" Company, Field "project" (Maybe Project)]

type Query
   = Object "Query" '[] '[ Argument "id" Text :> Field "environment" (Maybe Environment), Field "projects" (List Project), Argument "page" (Maybe Int32) :> Argument "pageSize" (Maybe Int32) :> Field "builds" (List Build), Argument "id" Text :> Field "project" (Maybe Project), Field "slackConfiguration" SlackConfiguration, Field "me" User, Field "onboarding" Onboarding]

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

getOnboarding :: Handler App Onboarding
getOnboarding = DB.getOnboarding >>= onboardingHandler

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

getSlackProjectIntegration :: DB.ProjectID -> Handler App (Maybe SlackProjectIntegration)
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

listBuilds :: Maybe DB.ProjectID -> Maybe Int32 -> Maybe Int32 -> Handler App (List Build)
listBuilds projectId page pageSize =
  map buildHandler <$> DB.listBuilds (fromIntegral $ fromMaybe 1 page, fromIntegral $ fromMaybe 20 pageSize) projectId

listEnvironments :: DB.ProjectID -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

-- Handlers
companyHandler :: DB.Company -> Handler App Company
companyHandler DB.Company {..} =
  pure $
  pure (DB.idText companyId) :<> pure companyName :<> pure companyOnboardingCompleted :<> pure companyCreatedAt :<>
  pure companyUpdatedAt

deploymentHandler :: DB.Deployment -> Handler App Deployment
deploymentHandler DB.Deployment {..} =
  pure $
  pure (DB.idText deploymentId) :<> pure (pure <$> deploymentStartedAt) :<> pure deploymentStatus :<>
  (DeploymentBuildD $ getBuild_ deploymentBuildId) :<>
  pure deploymentCreatedAt :<>
  pure deploymentUpdatedAt

environmentHandler :: DB.Environment -> Handler App Environment
environmentHandler DB.Environment {..} =
  pure $
  pure (DB.idText envId) :<> pure envName :<> pure envSlug :<> pure (map paramHandler envEnvVars) :<>
  getEnvLastDeployment envId :<>
  (EnvironmentProjectD $ getProject_ envProjectId) :<>
  pure envCreatedAt :<>
  pure envUpdatedAt

onboardingHandler :: DB.Onboarding -> Handler App Onboarding
onboardingHandler DB.Onboarding {..} = pure $ getCompany_ :<> maybe (pure Nothing) getProject onboardingProjectId

paramHandler :: (Text, Text) -> Handler App Param
paramHandler (key, value) = pure $ pure key :<> pure value

projectHandler :: DB.Project -> Handler App Project
projectHandler DB.Project {..} =
  pure $
  pure (DB.idText projectId) :<> pure projectName :<> pure projectSlug :<> pure projectDeploymentImage :<>
  pure projectAccessToken :<>
  listEnvironments projectId :<>
  (\x y -> map ProjectBuildD <$> listBuilds (Just projectId) x y) :<>
  getSlackProjectIntegration projectId :<>
  pure projectCreatedAt :<>
  pure projectUpdatedAt

buildHandler :: DB.Build -> Handler App Build
buildHandler DB.Build {..} =
  pure $
  pure (DB.idText buildId) :<> pure buildName :<> getProject_ buildProjectId :<> pure (map paramHandler buildParams) :<>
  pure (map paramHandler buildMetadata) :<>
  pure buildCreatedAt :<>
  pure buildUpdatedAt

slackProjectIntegrationHandler :: DB.SlackProjectIntegration -> Handler App SlackProjectIntegration
slackProjectIntegrationHandler DB.SlackProjectIntegration {..} = pure $ pure spiWorkspaceName

userHandler :: DB.User -> Handler App User
userHandler DB.User {..} =
  pure $
  pure (DB.idText userId) :<> pure userFirstName :<> pure userLastName :<> pure userEmail :<> getCompany_ :<>
  pure userCreatedAt :<>
  pure userUpdatedAt

handler :: Api.User -> Env -> Handler App Query
handler Api.User {..} Env {..} =
  pure $
  (getEnvironment . DB.ID) :<> listProjects :<> listBuilds Nothing :<> (getProject . DB.ID) :<>
  getSlackConfiguration slackSettings :<>
  getUser_ (DB.ID (Key.keyText userId)) :<>
  getOnboarding

newtype Request = Request
  { reqQuery :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request" $ \o -> do
      reqQuery <- o .: "query"
      return Request {..}

call :: Api.AuthenticatedUser -> Api.Handler Api.Value
call (Api.AuthenticatedUser user) = do
  Request {..} <- requireJsonBody
  appEnv <- getYesod
  env <- DB.buildEnv user appEnv
  toJSON <$> DB.run env (interpretQuery @Query (handler user appEnv) reqQuery Nothing Map.empty)

postGraphQLR :: Api.Handler Api.Value
postGraphQLR = userAuth call
