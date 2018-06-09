module Resources where

import RIO

import Data.Aeson hiding (json)

import Deployments.Domain (Project(..), projectNameText)
import Util.Key (keyText)

data ProjectResource = ProjectResource
  { resProjectId :: !Text
  , resProjectName :: !Text
  }

instance ToJSON ProjectResource where
  toJSON ProjectResource {..} =
    object ["id" .= resProjectId, "name" .= resProjectName]

projectResource :: Project -> ProjectResource
projectResource Project {..} =
  let resProjectId = keyText projectId
      resProjectName = projectNameText projectName
   in ProjectResource {..}
