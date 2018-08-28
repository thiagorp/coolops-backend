module Handlers.ConnectProjectWithSlack
  ( call
  ) where

import RIO hiding (optional)

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

data Fields =
  Code

instance HasFieldName Fields where
  fieldName field =
    case field of
      Code -> "code"

newtype Request = Request
  { reqCode :: Maybe Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqCode <- o .:? fieldName Code
      return Request {..}

builder :: User -> Text -> Request -> WebValidation App.Params
builder User {..} projectId Request {..} =
  App.Params <$> pure userCompanyId <*> pure projectId <*> code
  where
    code = required Code reqCode |>> valid

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  projectId <- param "project_id"
  appParams <- jsonData >>= parseRequest (builder user projectId)
  result <- lift $ App.call appParams
  case result of
    Right _ -> status created201
    Left App.ProjectNotFound -> status notFound404
    Left App.CouldNotExchangeCode -> status internalServerError500
