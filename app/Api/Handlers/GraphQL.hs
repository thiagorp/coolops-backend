{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.GraphQL where

import RIO hiding (Handler)
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Map as Map

import Web.Scotty.Trans hiding (Param)

import GraphQL
import GraphQL.API
import GraphQL.Resolver ((:<>)(..), Handler)

import Authorization (AuthenticatedUser(..))
import qualified GraphQL.Database as DB
import qualified GraphQL.Database.Types as DB
import Types (WebMonad)

type App = DB.App

type Project
   = Object "Project" '[] '[ Field "id" Text, Field "name" Text, Field "deployment_image" Text, Field "access_token" Text, Field "environments" (List Environment), Field "created_at" Int32, Field "updated_at" Int32]

type Environment
   = Object "Environment" '[] '[ Field "id" Text, Field "name" Text, Field "environment_variables" (List Param), Field "created_at" Int32, Field "updated_at" Int32]

type Build
   = Object "Build" '[] '[ Field "id" Text, Field "name" Text, Field "project" Project, Field "params" (List Param), Field "metadata" (List Param), Field "created_at" Int32, Field "updated_at" Int32]

type Param = Object "Param" '[] '[ Field "key" Text, Field "value" Text]

type Query
   = Object "Query" '[] '[ Field "projects" (List Project), Argument "page" (Maybe Int32) :> Argument "page_size" (Maybe Int32) :> Field "builds" (List Build)]

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

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

environmentHandler :: DB.Environment -> Handler App Environment
environmentHandler DB.Environment {..} =
  pure $
  pure (DB.idText envId) :<> pure envName :<> pure (map paramHandler envEnvVars) :<>
  pure envCreatedAt :<>
  pure envUpdatedAt

listEnvironments :: DB.ProjectID -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

buildHandler :: DB.Build -> Handler App Build
buildHandler DB.Build {..} =
  pure $
  pure (DB.idText buildId) :<> pure buildName :<> getProject_ buildProjectId :<>
  pure (map paramHandler buildParams) :<>
  pure (map paramHandler buildMetadata) :<>
  pure buildCreatedAt :<>
  pure buildUpdatedAt

listBuilds :: Maybe Int32 -> Maybe Int32 -> Handler App (List Build)
listBuilds page pageSize =
  map buildHandler <$>
  DB.listBuilds
    (fromIntegral $ fromMaybe 1 page, fromIntegral $ fromMaybe 20 pageSize)

handler :: Handler App Query
handler = pure $ listProjects :<> listBuilds

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  query <- decodeUtf8Lenient . LBS.toStrict <$> body
  appEnv <- lift ask
  env <- lift $ DB.buildEnv user appEnv
  result <-
    lift $ DB.run env (interpretQuery @Query handler query Nothing Map.empty)
  json result
