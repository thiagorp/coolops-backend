module Api.Handlers.Login
  ( postTokensR
  ) where

import Api.Import

import Auth.Domain
import qualified Auth.UseCases.Login as App

data Fields
  = Email
  | Password

instance HasFieldName Fields where
  fieldName field =
    case field of
      Email -> "email"
      Password -> "password"

data Request = Request
  { reqEmail :: !(Maybe Text)
  , reqPassword :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      reqEmail <- o .:? fieldName Email
      reqPassword <- o .:? fieldName Password
      return Request {..}

newtype Response = Response
  { resAccessToken :: Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["access_token" .= resAccessToken]

builder :: Request -> WebValidation App.Params
builder Request {..} = App.Params <$> email <*> password
  where
    email = required_ Email reqEmail |>> buildEmailAddress
    password = required_ Password reqPassword |>> buildPassword

buildResponse :: User -> Handler Response
buildResponse User {..} = do
  resAccessToken <- accessTokenTextM userAccessToken
  return Response {..}

postTokensR :: Handler Value
postTokensR = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder
  result <- App.login requestData
  case result of
    Just u -> toJSON <$> buildResponse u
    Nothing -> sendResponseStatus unauthorized401 ()
