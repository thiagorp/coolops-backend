module Handlers.CreateProject
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Deployments.Domain (Project(..), buildProjectName, projectNameText)
import qualified Deployments.UseCases.CreateProject as App
import Types
import Util.Key (keyText)
import Validation

data Fields =
  Name

instance HasFieldName Fields where
  fieldName field =
    case field of
      Name -> "name"

data Request = Request
  { reqProjectName :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqProjectName <- o .:? (fieldName Name)
      return Request {..}

data Response = Response
  { resProjectId :: !Text
  , resProjectName :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["id" .= resProjectId, "name" .= resProjectName]

builder :: User -> Request -> WebValidation App.CreateProject
builder (User {..}) (Request {..}) =
  App.CreateProject <$> projectName <*> (pure userCompanyId)
  where
    projectName = required Name reqProjectName >>> buildProjectName

buildResponse :: Project -> WebMonad Response
buildResponse Project {..} = do
  let resProjectId = keyText projectId
  let resProjectName = projectNameText projectName
  return Response {..}

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser uId) = do
  requestData <- jsonData >>= parseRequest (builder uId)
  project <- lift $ App.call requestData
  buildResponse project >>= json
