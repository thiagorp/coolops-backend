module Handlers.ConnectProjectWithSlack
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)

import Network.HTTP.Types.Status
  ( created201
  , internalServerError500
  , notFound404
  )
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import qualified Slack.UseCases.IntegrateProjectFromOAuth as App
import Types
import Validation

data Fields
  = Code
  | RedirectUri

instance HasFieldName Fields where
  fieldName field =
    case field of
      Code -> "code"
      RedirectUri -> "redirect_uri"

data Request = Request
  { reqCode :: !(Maybe Text)
  , reqRedirectUri :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqCode <- o .:? (fieldName Code)
      reqRedirectUri <- o .:? (fieldName RedirectUri)
      return Request {..}

builder :: User -> Text -> Request -> WebValidation App.Params
builder (User {..}) projectId (Request {..}) =
  App.Params <$> pure userCompanyId <*> pure projectId <*> code <*> redirectUri
  where
    code = required Code reqCode >>> valid
    redirectUri = required RedirectUri reqRedirectUri >>> valid

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projectId <- param "project_id"
  appParams <- jsonData >>= parseRequest (builder user projectId)
  result <- lift $ App.call appParams
  case result of
    Right _ -> status created201
    Left App.ProjectNotFound -> status notFound404
    Left App.CouldNotExchangeCode -> status internalServerError500
