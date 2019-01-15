{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Api.Handlers.GraphQL
  ( postGraphQLR
  ) where

import Api.Import hiding
  ( App
  , Build
  , Company
  , Deployment
  , Environment
  , Enum
  , Handler
  , Object
  , Project
  , SlackAccessToken
  , SlackProjectIntegration
  , User
  , UUID
  )
import qualified Api.Import as Api

import qualified RIO.HashMap as HashMap
import qualified RIO.Map as Map

import GraphQL
import GraphQL.API
import GraphQL.Resolver ((:<>)(..), Handler, HasResolver(..))

import qualified Common.Config as Config
import qualified GraphQL.Api.Types as Api
import qualified GraphQL.Database as DB
import GraphQL.Instances
import qualified Model as DB

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
   = Object "Project" '[]
   '[ Field "id" DB.ProjectId
    , Field "name" DB.ProjectName
    , Field "slug" DB.Slug
    , Field "deploymentImage" DB.DockerImage
    , Field "accessToken" DB.AccessToken
    , Field "environments" (List Environment)
    , Argument "page" (Maybe Int64) :> Argument "pageSize" (Maybe Int64) :> Field "builds" (List ProjectBuild)
    , Field "slackIntegration" (Maybe SlackProjectIntegration)
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

newtype EnvironmentProjectD m =
  EnvironmentProjectD (Handler m Project)

data EnvironmentProject

instance forall m. (Monad m) => HasResolver m EnvironmentProject where
  type Handler m EnvironmentProject = EnvironmentProjectD m
  resolve (EnvironmentProjectD rd) = resolve @m @Project rd

instance HasAnnotatedType EnvironmentProject where
  getAnnotatedType = getAnnotatedType @Int

type Environment
   = Object "Environment" '[]
   '[ Field "id" DB.EnvironmentId
    , Field "name" DB.EnvironmentName
    , Field "slug" DB.Slug
    , Field "environmentVariables" (List Param)
    , Field "lastDeployment" (Maybe Deployment)
    , Field "project" EnvironmentProject
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

type Build
   = Object "Build" '[]
   '[ Field "id" DB.BuildId
    , Field "name" DB.BuildName
    , Field "project" Project
    , Field "params" (List Param)
    , Field "metadata" (List Param)
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

type Company
   = Object "Company" '[]
   '[ Field "id" DB.CompanyId
    , Field "name" DB.CompanyName
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

newtype DeploymentBuildD m =
  DeploymentBuildD (Handler m Build)

data DeploymentBuild

instance forall m. (Monad m) => HasResolver m DeploymentBuild where
  type Handler m DeploymentBuild = DeploymentBuildD m
  resolve (DeploymentBuildD rd) = resolve @m @Build rd

instance HasAnnotatedType DeploymentBuild where
  getAnnotatedType = getAnnotatedType @Int

type Deployment
   = Object "Deployment" '[]
   '[ Field "id" DB.DeploymentId
    , Field "startedAt" (Maybe DB.UTCTime)
    , Field "status" (Enum "DeploymentStatus" DB.DeploymentStatus)
    , Field "build" DeploymentBuild
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

type Param
  = Object "Param" '[]
  '[ Field "key" Text
   , Field "value" Text
   ]

type SlackConfiguration = Object "SlackConfiguration" '[] '[ Field "clientId" Text]

type SlackChannel
  = Object "SlackChannel" '[]
  '[ Field "id" Text
   , Field "name" Text
   ]

type SlackAccessToken
  = Object "SlackAccessToken" '[]
  '[ Field "teamName" Text
   , Field "channels" (List SlackChannel)
   ]

type SlackProjectIntegration
  = Object "SlackProjectIntegration" '[]
  '[ Field "channelName" Text
   , Field "channelId" Text
   ]

type User
   = Object "User" '[]
   '[ Field "id" DB.UserId
    , Field "firstName" DB.UserName
    , Field "lastName" DB.UserName
    , Field "email" DB.EmailAddress
    , Field "company" Company
    , Field "createdAt" DB.UTCTime
    , Field "updatedAt" DB.UTCTime
    ]

type Query
   = Object "Query" '[]
   '[ Argument "id" UUID :> Field "environment" (Maybe Environment)
    , Field "projects" (List Project)
    , Argument "page" (Maybe Int64) :> Argument "pageSize" (Maybe Int64) :> Field "builds" (List Build)
    , Argument "id" UUID :> Field "project" (Maybe Project)
    , Field "slackAccessToken" (Maybe SlackAccessToken)
    , Field "slackConfiguration" SlackConfiguration
    , Field "me" User
    ]

-- Datasource
getBuild_ :: DB.BuildId -> Handler App Build
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

getEnvironment :: DB.EnvironmentId -> Handler App (Maybe Environment)
getEnvironment id_ = do
  maybe_ <- DB.getEnvironment id_
  case maybe_ of
    Just e -> pure $ Just (environmentHandler e)
    Nothing -> return Nothing

getProject_ :: DB.ProjectId -> Handler App Project
getProject_ pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> projectHandler p
    Nothing -> fail "Project not found"

getProject :: DB.ProjectId -> Handler App (Maybe Project)
getProject pId = do
  maybeProject <- DB.getProject pId
  case maybeProject of
    Just p -> pure $ Just (projectHandler p)
    Nothing -> return Nothing

getEnvLastDeployment :: DB.EnvironmentId -> Handler App (Maybe Deployment)
getEnvLastDeployment eId = do
  maybeDeployment <- DB.getEnvLastDeployment eId
  case maybeDeployment of
    Just d -> pure $ Just (deploymentHandler d)
    Nothing -> return Nothing

getSlackAccessToken :: Handler App (Maybe SlackAccessToken)
getSlackAccessToken = do
  maybe_ <- DB.getSlackAccessToken
  case maybe_ of
    Just at -> pure $ Just (slackAccessTokenHandler at)
    Nothing -> return Nothing

getSlackConfiguration :: Config.SlackSettings -> Handler App SlackConfiguration
getSlackConfiguration Config.SlackSettings {..} = pure $ pure slackClientId

getSlackProjectIntegration :: DB.ProjectId -> Handler App (Maybe SlackProjectIntegration)
getSlackProjectIntegration pId = do
  maybe_ <- DB.getSlackProjectIntegration pId
  case maybe_ of
    Just e -> pure $ Just (slackProjectIntegrationHandler e)
    Nothing -> return Nothing

getUser_ :: DB.UserId -> Handler App User
getUser_ pId = do
  maybe_ <- DB.getUser pId
  case maybe_ of
    Just e -> userHandler e
    Nothing -> fail "User not found"

listBuilds :: Maybe DB.ProjectId -> Maybe Int64 -> Maybe Int64 -> Handler App (List Build)
listBuilds projectId page pageSize =
  map buildHandler <$> DB.listBuilds (fromIntegral $ fromMaybe 1 page, fromIntegral $ fromMaybe 20 pageSize) projectId

listEnvironments :: DB.ProjectId -> Handler App (List Environment)
listEnvironments pId = map environmentHandler <$> DB.listEnvironments pId

listProjects :: Handler App (List Project)
listProjects = map projectHandler <$> DB.listProjects

listSlackChannels :: Text -> Handler App (List SlackChannel)
listSlackChannels token = map slackChannelHandler <$> DB.listSlackChannels token

-- Handlers
companyHandler :: DB.Entity DB.Company -> Handler App Company
companyHandler (DB.Entity companyId DB.Company {..}) =
  pure $
    pure companyId
    :<> pure companyName
    :<> pure companyCreatedAt
    :<> pure companyUpdatedAt

deploymentHandler :: DB.Entity DB.Deployment -> Handler App Deployment
deploymentHandler (DB.Entity deploymentId DB.Deployment {..}) =
  pure $
    pure deploymentId
    :<> pure (pure <$> deploymentStartedAt)
    :<> pure deploymentStatus
    :<> (DeploymentBuildD $ getBuild_ deploymentBuildId)
    :<> pure deploymentCreatedAt
    :<> pure deploymentUpdatedAt

environmentHandler :: DB.Entity DB.Environment -> Handler App Environment
environmentHandler (DB.Entity environmentId DB.Environment {..}) =
  pure $
    pure environmentId
    :<> pure environmentName
    :<> pure environmentSlug
    :<> pure (map paramHandler (HashMap.toList environmentEnvVars))
    :<> getEnvLastDeployment environmentId
    :<> (EnvironmentProjectD $ getProject_ environmentProjectId)
    :<> pure environmentCreatedAt
    :<> pure environmentUpdatedAt

paramHandler :: (Text, Text) -> Handler App Param
paramHandler (key, value) =
  pure $
    pure key
    :<> pure value

projectHandler :: DB.Entity DB.Project -> Handler App Project
projectHandler (DB.Entity projectId DB.Project {..}) =
  pure $
    pure projectId
    :<> pure projectName
    :<> pure projectSlug
    :<> pure projectDeploymentImage
    :<> pure projectAccessToken
    :<> listEnvironments projectId
    :<> (\x y -> map ProjectBuildD <$> listBuilds (Just projectId) x y)
    :<> getSlackProjectIntegration projectId
    :<> pure projectCreatedAt
    :<> pure projectUpdatedAt

buildHandler :: DB.Entity DB.Build -> Handler App Build
buildHandler (DB.Entity buildId DB.Build {..}) =
  pure $
    pure buildId
    :<> pure buildName
    :<> getProject_ buildProjectId
    :<> pure (map paramHandler (HashMap.toList buildParams))
    :<> pure (map paramHandler (HashMap.toList buildMetadata))
    :<> pure buildCreatedAt
    :<> pure buildUpdatedAt

slackAccessTokenHandler :: DB.Entity DB.SlackAccessToken -> Handler App SlackAccessToken
slackAccessTokenHandler (DB.Entity _ DB.SlackAccessToken {..}) =
  pure $
    pure slackAccessTokenTeamName
    :<> listSlackChannels slackAccessTokenBotAccessToken

slackChannelHandler :: Api.SlackChannel -> Handler App SlackChannel
slackChannelHandler Api.SlackChannel {..} =
  pure $
    pure slackChannelId
    :<> pure slackChannelName

slackProjectIntegrationHandler :: DB.Entity DB.SlackProjectIntegration -> Handler App SlackProjectIntegration
slackProjectIntegrationHandler (DB.Entity _ DB.SlackProjectIntegration {..}) =
  pure $
    pure slackProjectIntegrationChannelName
    :<> pure slackProjectIntegrationChannelId

userHandler :: DB.Entity DB.User -> Handler App User
userHandler (DB.Entity userId DB.User {..}) =
  pure $
    pure userId
    :<> pure userFirstName
    :<> pure userLastName
    :<> pure userEmail
    :<> getCompany_
    :<> pure userCreatedAt
    :<> pure userUpdatedAt

handler :: DB.Entity DB.User -> Env -> Handler App Query
handler user Env {..} =
  pure $
    getEnvironment . DB.EnvironmentKey
    :<> listProjects
    :<> listBuilds Nothing
    :<> getProject . DB.ProjectKey
    :<> getSlackAccessToken
    :<> getSlackConfiguration slackSettings
    :<> getUser_ (DB.entityKey user)

newtype Request = Request
  { reqQuery :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request" $ \o -> do
      reqQuery <- o .: "query"
      return Request {..}

call :: DB.Entity DB.User -> Api.Handler Api.Value
call user = do
  Request {..} <- requireJsonBody
  appEnv <- getYesod
  env <- DB.buildEnv user appEnv
  liftIO $ toJSON <$> DB.run env (interpretQuery @Query (handler user appEnv) reqQuery Nothing Map.empty)

postGraphQLR :: Api.Handler Api.Value
postGraphQLR = userAuth call
