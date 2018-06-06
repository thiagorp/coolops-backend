module Handlers.Login
  ( login
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (unauthorized401)
import Web.Scotty.Trans

import Auth.Domain
import qualified Auth.UseCases.Login as App
import Types
import Validation

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
      reqEmail <- o .:? (fieldName Email)
      reqPassword <- o .:? (fieldName Password)
      return Request {..}

data Response = Response
  { resAccessToken :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["access_token" .= resAccessToken]

builder :: Request -> WebValidation App.Login
builder Request {..} = App.Login <$> email <*> password
  where
    email = required Email reqEmail >>> buildEmailAddress
    password = required Password reqPassword >>> buildPassword

buildResponse :: User -> WebMonad Response
buildResponse User {..} = do
  resAccessToken <- accessTokenTextM userAccessToken
  return Response {..}

respond :: Maybe User -> WebMonad ()
respond result =
  case result of
    Just u -> buildResponse u >>= json
    Nothing -> status unauthorized401

login :: WebMonad ()
login = buildRequest builder >>= lift . App.login >>= respond
