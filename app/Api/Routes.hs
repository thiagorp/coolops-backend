module Routes
  ( routes
  ) where

import RIO

import Data.Aeson (ToJSON(..), (.=), object)
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip
import Web.Scotty.Trans

import qualified Handlers.ConnectProjectWithSlack as ConnectProjectWithSlack
import qualified Handlers.ConnectWithSlack as ConnectWithSlack
import qualified Handlers.CreateBuild as CreateBuild
import qualified Handlers.CreateDeployment as CreateDeployment
import qualified Handlers.CreateEnvironment as CreateEnvironment
import qualified Handlers.CreateProject as CreateProject
import qualified Handlers.DisconnectFromSlack as DisconnectFromSlack
import qualified Handlers.GetDeploymentLogs as GetDeploymentLogs
import qualified Handlers.GetEnvironment as GetEnvironment
import qualified Handlers.GetProfile as GetProfile
import qualified Handlers.GetProject as GetProject
import qualified Handlers.GetSlackConfig as GetSlackConfig
import qualified Handlers.GetSlackProjectIntegration as GetSlackProjectIntegration
import qualified Handlers.GraphQL as GraphQL
import qualified Handlers.HealthCheck as HealthCheck
import qualified Handlers.ListProjects as ListProjects
import qualified Handlers.Login as Handlers
import qualified Handlers.Signup as Handlers
import qualified Handlers.UpdateEnvironment as UpdateEnvironment
import qualified Handlers.UpdateProject as UpdateProject

import Authorization (projectAuth, userAuth)
import Common.App (AppT)
import Types (WebError(..))
import Validation (WebValidationError, validationToString)

newtype ValidationResponse =
  ValidationResponse WebValidationError

instance ToJSON ValidationResponse where
  toJSON (ValidationResponse errors) = object $ foldr buildObject [] errors
    where
      buildObject (name, e) obj = obj <> [name .= map validationToString e]

errorHandler :: Monad m => WebError -> ActionT WebError m ()
errorHandler e =
  case e of
    ValidationError validationErrors -> do
      status status422
      json $ ValidationResponse validationErrors
    StrError str -> do
      status status500
      json str

corsMiddleware :: Middleware
corsMiddleware = cors $ const (Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type", "authorization"]
        , corsMethods = ["GET", "HEAD", "POST", "PATCH", "DELETE"]
        }

routes :: Middleware -> ScottyT WebError AppT ()
routes logger = do
  middleware corsMiddleware
  middleware logger
  middleware $ gzip def
  defaultHandler errorHandler
  post "/graphql" $ userAuth GraphQL.call
  get "/health" HealthCheck.call
  post "/signup" Handlers.signup
  post "/tokens" Handlers.login
  get "/me" $ userAuth GetProfile.call
  post "/projects" $ userAuth CreateProject.call
  get "/projects" $ userAuth ListProjects.call
  get "/projects/:id" $ userAuth GetProject.call
  patch "/projects/:id" $ userAuth UpdateProject.call
  post "/projects/:project_id/environments" $ userAuth CreateEnvironment.call
  post "/projects/:project_id/slack_integration" $
    userAuth ConnectProjectWithSlack.call
  get "/projects/:project_id/slack_integration" $
    userAuth GetSlackProjectIntegration.call
  get "/environments/:id" $ userAuth GetEnvironment.call
  patch "/environments/:id" $ userAuth UpdateEnvironment.call
  post "/builds" $ projectAuth CreateBuild.call
  post "/deployments" $ userAuth CreateDeployment.call
  get "/deployments/:id/logs" GetDeploymentLogs.call
  post "/slack_config" $ userAuth ConnectWithSlack.call
  get "/slack_config" $ userAuth GetSlackConfig.call
  delete "/slack_config" $ userAuth DisconnectFromSlack.call
