module GraphQL.Database.Types where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson (Result(..), Value, fromJSON)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GraphQL.API.Enum
import GraphQL.Value.ToValue

import Deployments.Database.Deployment (DbStatus(..))

type Param = (Text, Text)

parseParams :: Value -> [Param]
parseParams value =
  case fromJSON value of
    Error _ -> []
    Success p -> HashMap.toList p

newtype ID a =
  ID Text
  deriving (Hashable, Eq, Show, ToValue, ToField)

instance FromField (ID a) where
  fromField f bs = ID . UUID.toText <$> fromField f bs

idText :: ID a -> Text
idText (ID t) = t

type ProjectID = ID Text

data Project = Project
  { projectId :: ProjectID
  , projectName :: Text
  , projectDeploymentImage :: Text
  , projectAccessToken :: Text
  , projectCreatedAt :: Int32
  , projectUpdatedAt :: Int32
  } deriving (Show)

instance FromRow Project where
  fromRow = do
    projectId <- field
    projectName <- field
    projectDeploymentImage <- field
    projectAccessToken <- field
    projectCreatedAt <- field
    projectUpdatedAt <- field
    return Project {..}

type EnvironmentID = ID Environment

data Environment = Environment
  { envId :: EnvironmentID
  , envName :: Text
  , envEnvVars :: [Param]
  , envProjectId :: ProjectID
  , envCreatedAt :: Int32
  , envUpdatedAt :: Int32
  } deriving (Show)

instance FromRow Environment where
  fromRow = do
    envId <- field
    envName <- field
    envEnvVars <- parseParams <$> field
    envProjectId <- field
    envCreatedAt <- field
    envUpdatedAt <- field
    return Environment {..}

type BuildID = ID Build

data Build = Build
  { buildId :: BuildID
  , buildName :: Text
  , buildParams :: [Param]
  , buildMetadata :: [Param]
  , buildProjectId :: ProjectID
  , buildCreatedAt :: Int32
  , buildUpdatedAt :: Int32
  } deriving (Show)

instance FromRow Build where
  fromRow = do
    buildId <- field
    buildName <- field
    buildParams <- parseParams <$> field
    buildMetadata <- parseParams <$> field
    buildProjectId <- field
    buildCreatedAt <- field
    buildUpdatedAt <- field
    return Build {..}

type DeploymentID = ID Deployment

data DeploymentStatus
  = QUEUED
  | RUNNING
  | SUCCEEDED
  | FAILED
  deriving (Generic, Show)

instance FromField DeploymentStatus where
  fromField f bs = do
    status <- fromField f bs
    case status of
      DbQueued -> return QUEUED
      DbRunning -> return RUNNING
      DbSucceeded -> return SUCCEEDED
      DbFailed _ -> return FAILED

instance GraphQLEnum DeploymentStatus

data Deployment = Deployment
  { deploymentId :: DeploymentID
  , deploymentStartedAt :: Maybe Int32
  , deploymentEnvId :: EnvironmentID
  , deploymentBuildId :: BuildID
  , deploymentStatus :: DeploymentStatus
  , deploymentCreatedAt :: Int32
  , deploymentUpdatedAt :: Int32
  } deriving (Show)

instance FromRow Deployment where
  fromRow = do
    deploymentId <- field
    deploymentStartedAt <- field
    deploymentEnvId <- field
    deploymentBuildId <- field
    deploymentStatus <- field
    deploymentCreatedAt <- field
    deploymentUpdatedAt <- field
    return Deployment {..}

newtype SlackConfiguration = SlackConfiguration
  { slackConfigurationClientId :: Text
  } deriving (Show)

data SlackProjectIntegration = SlackProjectIntegration
  { spiProjectId :: ProjectID
  , spiWorkspaceName :: Text
  } deriving (Show)

instance FromRow SlackProjectIntegration where
  fromRow = do
    spiProjectId <- field
    spiWorkspaceName <- field
    return SlackProjectIntegration {..}

type UserID = ID User

data User = User
  { userId :: UserID
  , userFirstName :: Text
  , userLastName :: Text
  , userEmail :: Text
  , userCompanyId :: DBCompanyID
  , userCreatedAt :: Int32
  , userUpdatedAt :: Int32
  } deriving (Generic, Show)

instance FromRow User

type DBCompanyID = ID Company

data Company = Company
  { companyId :: DBCompanyID
  , companyName :: Text
  , companyOnboardingCompleted :: Bool
  , companyCreatedAt :: Int32
  , companyUpdatedAt :: Int32
  } deriving (Generic, Show)

instance FromRow Company
