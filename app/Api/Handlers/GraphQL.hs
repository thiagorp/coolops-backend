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

import Authorization (AuthenticatedUser(..))
import qualified GraphQL.Database as DB
import qualified GraphQL.Database.Types as DB
import Types (WebMonad)

type App = DB.App

type Project
   = Object "Project" '[] '[ Field "id" Text, Field "name" Text, Field "deploymentImage" Text, Field "accessToken" Text, Field "environments" (List Environment), Field "createdAt" Int32, Field "updatedAt" Int32]

type Environment
   = Object "Environment" '[] '[ Field "id" Text, Field "name" Text, Field "environmentVariables" (List Param), Field "lastDeployment" (Maybe Deployment), Field "createdAt" Int32, Field "updatedAt" Int32]

type Build
   = Object "Build" '[] '[ Field "id" Text, Field "name" Text, Field "project" Project, Field "params" (List Param), Field "metadata" (List Param), Field "createdAt" Int32, Field "updatedAt" Int32]

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

type Query
   = Object "Query" '[] '[ Field "projects" (List Project), Argument "page" (Maybe Int32) :> Argument "pageSize" (Maybe Int32) :> Field "builds" (List Build)]

getBuild_ :: DB.BuildID -> Handler App Build
getBuild_ id_ = do
  maybeBuild <- DB.getBuild id_
  case maybeBuild of
    Just b -> buildHandler b
    Nothing -> fail "Build not found"

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

listBuilds :: Maybe Int32 -> Maybe Int32 -> Handler App (List Build)
listBuilds page pageSize =
  map buildHandler <$>
  DB.listBuilds
    (fromIntegral $ fromMaybe 1 page, fromIntegral $ fromMaybe 20 pageSize)

listEnvironments :: DB.ProjectID -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

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

handler :: Handler App Query
handler = pure $ listProjects :<> listBuilds

newtype Request = Request
  { reqQuery :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request" $ \o -> do
      reqQuery <- o .: "query"
      return Request {..}

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  Request {..} <- jsonData
  appEnv <- lift ask
  env <- lift $ DB.buildEnv user appEnv
  result <-
    lift $ DB.run env (interpretQuery @Query handler reqQuery Nothing Map.empty)
  json result
