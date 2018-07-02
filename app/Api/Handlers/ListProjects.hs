module Handlers.ListProjects
  ( call
  ) where

import RIO
import qualified RIO.List as List

import Data.Aeson hiding (json)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import Database.Queries.ProjectsWithEnvironments
import Types (WebMonad)

newtype Response =
  Response Project

buildToJson :: Build -> Value
buildToJson Build {..} = object ["id" .= buildId, "name" .= buildName]

deploymentStatusToJson :: DbStatus -> Value
deploymentStatusToJson dbStatus =
  case dbStatus of
    DbRunning -> "running"
    DbFailed _ -> "failed"
    DbQueued -> "queued"
    DbSucceeded -> "succeeded"

deploymentToJson :: Deployment -> Value
deploymentToJson Deployment {..} =
  object
    [ "id" .= deploymentId
    , "last_changed_at" .= deploymentLastChangedAt
    , "status" .= deploymentStatusToJson deploymentStatus
    , "build" .= buildToJson deploymentBuild
    ]

environmentToJson :: Environment -> Value
environmentToJson Environment {..} =
  object
    [ "id" .= environmentId
    , "name" .= environmentName
    , "current_deployment" .=
      maybe Null deploymentToJson environmentCurrentDeployment
    ]

instance ToJSON Response where
  toJSON (Response Project {..}) =
    object
      [ "id" .= projectId
      , "name" .= projectName
      , "deployment_image" .= projectDeploymentImage
      , "access_token" .= projectAccessToken
      , "environments" .= map environmentToJson projectEnvironments
      ]

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projects <- lift $ listProjectsWithEnvironments (userCompanyId user)
  let sortedProjects =
        List.sortBy
          (\p1 p2 -> compare (projectName p1) (projectName p2))
          projects
  json $ Response <$> sortedProjects
