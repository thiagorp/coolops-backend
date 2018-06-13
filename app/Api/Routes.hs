module Routes
  ( routes
  ) where

import RIO

import Data.Aeson (ToJSON(..), (.=), object)
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Web.Scotty.Trans

import qualified Handlers.CreateProject as CreateProject
import qualified Handlers.GetProject as GetProject
import qualified Handlers.ListProjects as ListProjects
import qualified Handlers.Login as Handlers
import qualified Handlers.Signup as Handlers
import qualified Handlers.UpdateProject as UpdateProject

import Authorization (userAuth)
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
        {corsRequestHeaders = ["content-type", "authorization"]}

routes :: ScottyT WebError AppT ()
routes = do
  middleware corsMiddleware
  defaultHandler errorHandler
  post "/signup" Handlers.signup
  post "/tokens" Handlers.login
  post "/projects" $ userAuth CreateProject.call
  get "/projects" $ userAuth ListProjects.call
  get "/projects/:id" $ userAuth GetProject.call
  patch "/projects/:id" $ userAuth UpdateProject.call
