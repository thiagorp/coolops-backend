module Deployments.Domain
  ( Project(..)
  , ProjectID
  , ProjectName
  , ProjectDeploymentImage
  , CompanyID
  , buildProjectDeploymentImage
  , buildProjectName
  , genProjectId
  , projectDeploymentImageText
  , projectNameText
  ) where

import RIO

import Auth.Domain (CompanyID)
import Util.Key
import Util.Validation

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField

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

data Project = Project
  { projectId :: !ProjectID
  , projectName :: !ProjectName
  , projectDeploymentImage :: !ProjectDeploymentImage
  , projectCompanyId :: !CompanyID
  }

genProjectId :: MonadIO m => m ProjectID
genProjectId = genID

buildProjectName :: Text -> Validated ProjectName
buildProjectName name = ProjectName <$> validateMinLength 1 name

projectNameText :: ProjectName -> Text
projectNameText (ProjectName name) = name

buildProjectDeploymentImage :: Text -> Validated ProjectDeploymentImage
buildProjectDeploymentImage image =
  ProjectDeploymentImage <$> validateMinLength 1 image

projectDeploymentImageText :: ProjectDeploymentImage -> Text
projectDeploymentImageText (ProjectDeploymentImage text) = text
