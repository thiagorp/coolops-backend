module Deployments.Domain.Build
  ( Build(..)
  , ID
  , Name
  , ProjectID
  , genId
  , buildBuildName
  , nameText
  ) where

import RIO

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

import qualified Deployments.Domain.Project as Project
import Util.Key
import Util.Validation

type ID = Key Build

type ProjectID = Project.ID

newtype Name =
  Name Text

instance ToField Name where
  toField (Name name) = toField name

instance FromField Name where
  fromField f bs = Name <$> (fromField f bs)

data Build = Build
  { buildId :: !ID
  , buildName :: !Name
  , buildParams :: !(HashMap Text Text)
  , buildProjectId :: !ProjectID
  }

genId :: MonadIO m => m ID
genId = genID

buildBuildName :: Text -> Validated Name
buildBuildName name = Name <$> validateMinLength 1 name

nameText :: Name -> Text
nameText (Name name) = name