module Api.Handlers.Login
  ( login
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (unauthorized401)
import Web.Scotty.Trans

import Api.Types
import Api.Validation
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
      reqEmail <- o .:? (fieldName Email)
      reqPassword <- o .:? (fieldName Password)
      return Request {..}

data Response = Response
  { resAccessToken :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} = object ["access_token" .= resAccessToken]

parseRequest :: Request -> WebMonad App.Login
parseRequest Request {..} =
  case p of
    Invalid e -> raise (ValidationError e)
    Valid s -> return s
  where
    email =
      onField Email $ validateRequired reqEmail `andThen` buildEmailAddress
    password =
      onField Password $ validateRequired reqPassword `andThen` buildPassword
    p = App.Login <$> email <*> password

buildParams :: WebMonad App.Login
buildParams = jsonData >>= parseRequest

buildResponse :: User -> WebMonad Response
buildResponse User {..} = do
  token <- accessTokenTextM userAccessToken
  return $ Response {resAccessToken = token}

login :: WebMonad ()
login = do
  requestData <- buildParams
  result <- lift $ App.login requestData
  case result of
    Just u -> buildResponse u >>= json
    Nothing -> status unauthorized401
