{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.GraphQL where

import RIO hiding (Handler)
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map

import Web.Scotty.Trans hiding (Param)

import GraphQL
import GraphQL.API
import GraphQL.Resolver ((:<>)(..), Handler)

import Authorization (AuthenticatedUser(..))
import qualified Deployments.Domain.Build as BB
import qualified Deployments.Domain.Environment as BE
import qualified Deployments.Domain.Project as BP
import qualified GraphQL.Database as DB
import Types (WebMonad)

type App = DB.App

type Project
   = Object "Project" '[] '[ Field "id" Text, Field "name" Text, Field "deployment_image" Text, Field "access_token" Text, Field "environments" (List Environment)]

type Environment
   = Object "Environment" '[] '[ Field "id" Text, Field "name" Text, Field "environment_variables" (List Param)]

type Build
   = Object "Build" '[] '[ Field "id" Text, Field "name" Text, Field "project" Project, Field "params" (List Param), Field "metadata" (List Param)]

type Param = Object "Param" '[] '[ Field "key" Text, Field "value" Text]

type Query
   = Object "Query" '[] '[ Field "projects" (List Project), Argument "page" (Maybe Int32) :> Argument "page_size" (Maybe Int32) :> Field "builds" (List Build)]

paramHandler :: (Text, Text) -> Handler App Param
paramHandler (key, value) = pure $ pure key :<> pure value

projectHandler :: BP.Project -> Handler App Project
projectHandler project =
  pure $
  pure (BP.projectId_ project) :<> pure (BP.projectName_ project) :<>
  pure (BP.projectDeploymentImage_ project) :<>
  pure (BP.projectAccessToken_ project) :<>
  listEnvironments (BP.projectId project)

getProject_ :: BP.ID -> Handler App Project
getProject_ pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> projectHandler p
    Nothing -> fail "Project not found"

getProject :: BP.ID -> Handler App (Maybe Project)
getProject pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> pure $ Just (projectHandler p)
    Nothing -> return Nothing

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

environmentHandler :: BE.Environment -> Handler App Environment
environmentHandler e =
  pure $
  pure (BE.environmentId_ e) :<> pure (BE.environmentName_ e) :<>
  pure (map paramHandler (HashMap.toList (BE.environmentEnvVars e)))

listEnvironments :: BP.ID -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

buildHandler :: BB.Build -> Handler App Build
buildHandler build =
  pure $
  pure (BB.buildId_ build) :<> pure (BB.buildName_ build) :<>
  getProject_ (BB.buildProjectId build) :<>
  pure (map paramHandler (HashMap.toList (BB.buildParams build))) :<>
  pure (map paramHandler (HashMap.toList (BB.buildMetadata build)))

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
