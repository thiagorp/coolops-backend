module Handlers.ConnectWithSlack
  ( call
  ) where

import RIO

import Data.Aeson hiding (json)

import Network.HTTP.Types.Status (created201, internalServerError500)
import Web.Scotty.Trans

import Authorization (AuthenticatedUser(..), User(..))
import qualified Slack.UseCases.CreateTeamFromOAuth as App
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

builder :: User -> Request -> WebValidation App.Params
builder User {..} Request {..} = App.Params <$> pure userCompanyId <*> code
  where
    code = required Code reqCode >>> valid

call :: AuthenticatedUser -> WebMonad ()
call (AuthenticatedUser user) = do
  appParams <- jsonData >>= parseRequest (builder user)
  result <- lift $ App.call appParams
  case result of
    Right _ -> status created201
    Left App.CouldNotExchangeCode -> status internalServerError500
