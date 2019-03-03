{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handlers.Login
  ( postTokensR
  ) where

import Api.Import

import qualified Auth.UseCases.Login as App

data Request = Request
  { reqEmail :: !EmailAddress
  , reqPassword :: !Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "" $ \o -> do
      reqEmail <- o .: "email"
      reqPassword <- o .: "password"
      return Request {..}

newtype Response = Response
  { resAccessToken :: AccessToken
  }

instance ToJSON Response where
  toJSON Response {..} = object ["access_token" .= resAccessToken]

mapParams :: Request -> App.Params
mapParams Request {..} = App.Params reqEmail reqPassword

buildResponse :: User -> Response
buildResponse User {..} = Response {resAccessToken = userAccessToken}

postTokensR :: Handler Value
postTokensR = do
  requestData <- mapParams <$> requireJsonBody
  result <- runAppInHandler $ App.login requestData
  case result of
    Just u -> return $ toJSON $ buildResponse u
    Nothing -> sendResponseStatus unauthorized401 ()
