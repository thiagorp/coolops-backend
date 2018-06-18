module Deployments.Domain
  ( Build(..)
  , BuildID
  , BuildName
  , Environment(..)
  , EnvironmentID
  , EnvironmentName
  , Project(..)
  , ProjectID
  , ProjectName
  , ProjectDeploymentImage
  , ProjectAccessToken
  , CompanyID
  , buildBuildName
  , buildEnvironmentName
  , buildProjectDeploymentImage
  , buildProjectName
  , buildNameText
  , environmentNameText
  , genBuildId
  , genEnvironmentId
  , genProjectAccessToken
  , genProjectId
  , projectDeploymentImageText
  , projectAccessTokenText
  , projectNameText
  ) where

import RIO

import Auth.Domain (CompanyID)
import Util.Key
import Util.Validation

import Data.ByteString.Base58 (encodeBase58I, rippleAlphabet)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField
import System.Random (randomRIO)

type ProjectID = Key Project

newtype ProjectName =
  ProjectName Text

instance ToField ProjectName where
  toField (ProjectName name) = toField name

instance FromField ProjectName where
  fromField f bs = ProjectName <$> (fromField f bs)

newtype ProjectDeploymentImage =
  ProjectDeploymentImage Text

instance ToField ProjectDeploymentImage where
  toField (ProjectDeploymentImage image) = toField image

instance FromField ProjectDeploymentImage where
  fromField f bs = ProjectDeploymentImage <$> (fromField f bs)

newtype ProjectAccessToken =
  ProjectAccessToken ByteString

instance ToField ProjectAccessToken where
  toField (ProjectAccessToken token) = toField token

instance FromField ProjectAccessToken where
  fromField f bs = ProjectAccessToken <$> (fromField f bs)

data Project = Project
  { projectId :: !ProjectID
  , projectName :: !ProjectName
  , projectDeploymentImage :: !ProjectDeploymentImage
  , projectCompanyId :: !CompanyID
  , projectAccessToken :: !ProjectAccessToken
  }

type EnvironmentID = Key Environment

newtype EnvironmentName =
  EnvironmentName Text

instance ToField EnvironmentName where
  toField (EnvironmentName name) = toField name

instance FromField EnvironmentName where
  fromField f bs = EnvironmentName <$> (fromField f bs)

data Environment = Environment
  { environmentId :: !EnvironmentID
  , environmentName :: !EnvironmentName
  , environmentEnvVars :: !(HashMap Text Text)
  , environmentProjectId :: !ProjectID
  }

type BuildID = Key Build

newtype BuildName =
  BuildName Text

instance ToField BuildName where
  toField (BuildName name) = toField name

instance FromField BuildName where
  fromField f bs = BuildName <$> (fromField f bs)

data Build = Build
  { buildId :: !BuildID
  , buildName :: !BuildName
  , buildParams :: !(HashMap Text Text)
  , buildProjectId :: !ProjectID
  }

genProjectId :: MonadIO m => m ProjectID
genProjectId = genID

genEnvironmentId :: MonadIO m => m EnvironmentID
genEnvironmentId = genID

genBuildId :: MonadIO m => m BuildID
genBuildId = genID

buildProjectName :: Text -> Validated ProjectName
buildProjectName name = ProjectName <$> validateMinLength 1 name

projectNameText :: ProjectName -> Text
projectNameText (ProjectName name) = name

projectAccessTokenText :: ProjectAccessToken -> Text
projectAccessTokenText (ProjectAccessToken value) =
  case decodeUtf8' value of
    Right t -> t
    Left e -> error (show e)

buildProjectDeploymentImage :: Text -> Validated ProjectDeploymentImage
buildProjectDeploymentImage image =
  ProjectDeploymentImage <$> validateMinLength 1 image

projectDeploymentImageText :: ProjectDeploymentImage -> Text
projectDeploymentImageText (ProjectDeploymentImage text) = text

buildEnvironmentName :: Text -> Validated EnvironmentName
buildEnvironmentName name = EnvironmentName <$> validateMinLength 1 name

environmentNameText :: EnvironmentName -> Text
environmentNameText (EnvironmentName name) = name

buildBuildName :: Text -> Validated BuildName
buildBuildName name = BuildName <$> validateMinLength 1 name

buildNameText :: BuildName -> Text
buildNameText (BuildName name) = name

genProjectAccessToken :: (MonadIO m) => m ProjectAccessToken
genProjectAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m ProjectAccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ ProjectAccessToken token
    _ -> genAccessToken' (size - 1) (token <> randomChar)
