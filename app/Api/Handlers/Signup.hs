module Handlers.Signup
  ( signup
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (status409)
import Web.Scotty.Trans

import Auth.Domain
import qualified Auth.UseCases.Signup as App
import Types
import Validation

data Fields
  = FirstName
  | LastName
  | Email
  | Password
  | CompanyName

instance HasFieldName Fields where
  fieldName field =
    case field of
      FirstName -> "first_name"
      LastName -> "last_name"
      Email -> "email"
      Password -> "password"
      CompanyName -> "company_name"

data Request = Request
  { reqFirstName :: !(Maybe Text)
  , reqLastName :: !(Maybe Text)
  , reqEmail :: !(Maybe Text)
  , reqPassword :: !(Maybe Text)
  , reqCompanyName :: !(Maybe Text)
  }

instance FromJSON Request where
  parseJSON =
    withObject "signup params" $ \o -> do
      reqFirstName <- o .:? fieldName FirstName
      reqLastName <- o .:? fieldName LastName
      reqEmail <- o .:? fieldName Email
      reqPassword <- o .:? fieldName Password
      reqCompanyName <- o .:? fieldName CompanyName
      return Request {..}

data Response = Response
  { resUserToken :: !Text
  , resCompanyToken :: !Text
  , resUserId :: !Text
  , resCompanyId :: !Text
  }

instance ToJSON Response where
  toJSON Response {..} =
    object
      [ "user_access_token" .= resUserToken
      , "user_id" .= resUserId
      , "company_access_token" .= resCompanyToken
      , "company_id" .= resCompanyId
      ]

builder :: Request -> WebValidation App.Params
builder Request {..} =
  App.Params <$> firstName <*> lastName <*> email <*> password <*> companyName
  where
    password = required Password reqPassword |>> buildPassword
    firstName = required FirstName reqFirstName |>> buildUserName
    lastName = required LastName reqLastName |>> buildUserName
    email = required Email reqEmail |>> buildEmailAddress
    companyName = required CompanyName reqCompanyName |>> buildCompanyName

buildResponse :: (User, Company) -> WebMonad Response
buildResponse (User {..}, Company {..}) = do
  resUserToken <- accessTokenTextM userAccessToken
  resCompanyToken <- accessTokenTextM companyToken
  let resUserId = keyText userId
  let resCompanyId = keyText companyId
  return Response {..}

signup :: WebMonad ()
signup = do
  requestData <- jsonData >>= parseRequest builder
  result <- lift $ App.signup requestData
  case result of
    Left App.UserAlreadyExists -> status status409 >> text "User already exists"
    Right value -> buildResponse value >>= json
