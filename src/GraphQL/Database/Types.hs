module GraphQL.Database.Types where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Aeson (Result(..), Value, fromJSON)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple.FromRow

type Param = (Text, Text)

parseParams :: Value -> [Param]
parseParams value =
  case fromJSON value of
    Error _ -> []
    Success p -> HashMap.toList p

data Project = Project
  { projectId :: !Text
  , projectName :: !Text
  , projectDeploymentImage :: !Text
  , projectAccessToken :: !Text
  , projectCreatedAt :: !Int32
  , projectUpdatedAt :: !Int32
  } deriving (Show)

instance FromRow Project where
  fromRow = do
    projectId <- UUID.toText <$> field
    projectName <- field
    projectDeploymentImage <- field
    projectAccessToken <- field
    projectCreatedAt <- field
    projectUpdatedAt <- field
    return Project {..}

data Environment = Environment
  { envId :: !Text
  , envName :: !Text
  , envEnvVars :: ![Param]
  , envProjectId :: !Text
  , envCreatedAt :: !Int32
  , envUpdatedAt :: !Int32
  } deriving (Show)

instance FromRow Environment where
  fromRow = do
    envId <- UUID.toText <$> field
    envName <- field
    envEnvVars <- parseParams <$> field
    envProjectId <- UUID.toText <$> field
    envCreatedAt <- field
    envUpdatedAt <- field
    return Environment {..}

data Build = Build
  { buildId :: !Text
  , buildName :: !Text
  , buildParams :: ![Param]
  , buildMetadata :: ![Param]
  , buildProjectId :: !Text
  , buildCreatedAt :: !Int32
  , buildUpdatedAt :: !Int32
  } deriving (Show)

instance FromRow Build where
  fromRow = do
    buildId <- UUID.toText <$> field
    buildName <- field
    buildParams <- parseParams <$> field
    buildMetadata <- parseParams <$> field
    buildProjectId <- UUID.toText <$> field
    buildCreatedAt <- field
    buildUpdatedAt <- field
    return Build {..}
