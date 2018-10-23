module Deployments.Domain.Build
  ( Build(..)
  , ID
  , Name
  , ProjectID
  , buildId_
  , buildName_
  , genId
  , nameText
  ) where

import RIO

import qualified Deployments.Domain.Project as Project
import Util.Key
import Util.Validation

type ID = Key Build

type ProjectID = Project.ID

type Name = Validated (SizeGreaterThan 1) Text

data Build = Build
  { buildId :: !ID
  , buildName :: !Name
  , buildParams :: !(HashMap Text Text)
  , buildMetadata :: !(HashMap Text Text)
  , buildProjectId :: !ProjectID
  } deriving (Show)

genId :: MonadIO m => m ID
genId = genID

buildId_ :: Build -> Text
buildId_ Build {..} = keyText buildId

nameText :: Name -> Text
nameText = getValue

buildName_ :: Build -> Text
buildName_ Build {..} = nameText buildName
