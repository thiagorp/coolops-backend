module Deployments.Domain.Environment
  ( ID
  , ProjectID
  , Name
  , Environment(..)
  , Slug
  , genId
  , environmentId_
  , buildName
  , environmentName_
  , buildSlug
  , nameText
  ) where

import RIO

import qualified Deployments.Domain.Project as Project
import Util.Key
import Util.Slug
import Util.Validation

import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))

type ID = Key Environment

type ProjectID = Project.ID

newtype Name =
  Name Text
  deriving (Show, ToField, FromField)

data Environment = Environment
  { environmentId :: !ID
  , environmentName :: !Name
  , environmentEnvVars :: !(HashMap Text Text)
  , environmentProjectId :: !ProjectID
  , environmentSlug :: !Slug
  } deriving (Show)

genId :: MonadIO m => m ID
genId = genID

environmentId_ :: Environment -> Text
environmentId_ Environment {..} = keyText environmentId

buildName :: Text -> Validated Name
buildName name = Name <$> validateMinLength 1 name

environmentName_ :: Environment -> Text
environmentName_ Environment {..} = nameText environmentName

buildSlug :: Text -> Validated Slug
buildSlug = validateSlug

nameText :: Name -> Text
nameText (Name name) = name
