module Api.Handlers.Login
  ( postTokensR
  ) where

import Api.Import

import Auth.Domain
import qualified Auth.UseCases.Login as App

data Request = Request
  { reqEmail :: !UserEmail
  , reqPassword :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      reqEmail <- o .: "email"
      reqPassword <- o .: "password"
      return Request {..}

newtype Response = Response
  { resAccessToken :: Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["access_token" .= resAccessToken]

mapParams :: Request -> App.Params
mapParams Request {..} = App.Params reqEmail reqPassword

buildResponse :: User -> Handler Response
buildResponse User {..} = do
  resAccessToken <- accessTokenTextM userAccessToken
  return Response {..}

postTokensR :: Handler Value
postTokensR = do
  requestData <- mapParams <$> requireJsonBody
  result <- App.login requestData
  case result of
    Just u -> toJSON <$> buildResponse u
    Nothing -> sendResponseStatus unauthorized401 ()
