module Deployments.Domain.Environment where

import RIO

import qualified Deployments.Domain.Project as Project
import Util.Key
import Util.Validation

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

type ID = Key Environment

type ProjectID = Project.ID

newtype Name =
  Name Text
  deriving (ToField, FromField)

data Environment = Environment
  { environmentId :: !ID
  , environmentName :: !Name
  , environmentEnvVars :: !(HashMap Text Text)
  , environmentProjectId :: !ProjectID
  }

genId :: MonadIO m => m ID
genId = genID

buildName :: Text -> Validated Name
buildName name = Name <$> validateMinLength 1 name

nameText :: Name -> Text
nameText (Name name) = name
