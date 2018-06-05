module Api.Handlers.Signup
  ( signup
  ) where

import RIO

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status (status409)
import Web.Scotty.Trans

import Api.Types
import Api.Validation
import Auth.Domain
import qualified Auth.UseCases.Signup as App

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
      reqFirstName <- o .:? (fieldName FirstName)
      reqLastName <- o .:? (fieldName LastName)
      reqEmail <- o .:? (fieldName Email)
      reqPassword <- o .:? (fieldName Password)
      reqCompanyName <- o .:? (fieldName CompanyName)
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

parseRequest :: Request -> WebMonad App.Signup
parseRequest Request {..} =
  case p of
    Invalid e -> raise (ValidationError e)
    Valid s -> return s
  where
    password =
      onField Password $ validateRequired reqPassword `andThen` buildPassword
    firstName =
      onField FirstName $ validateRequired reqFirstName `andThen` buildUserName
    lastName =
      onField LastName $ validateRequired reqLastName `andThen` buildUserName
    email =
      onField Email $ validateRequired reqEmail `andThen` buildEmailAddress
    companyName =
      onField CompanyName $
      validateRequired reqCompanyName `andThen` buildCompanyName
    p =
      App.Signup <$> firstName <*> lastName <*> email <*> password <*>
      companyName

buildParams :: WebMonad App.Signup
buildParams = jsonData >>= parseRequest

buildResponse :: (User, Company) -> WebMonad Response
buildResponse (User {..}, Company {..}) = do
  uToken <- accessTokenTextM userAccessToken
  cToken <- accessTokenTextM companyToken
  let uId = keyText userId
  let cId = keyText companyId
  return $
    Response
      { resUserId = uId
      , resUserToken = uToken
      , resCompanyId = cId
      , resCompanyToken = cToken
      }

signup :: WebMonad ()
signup = do
  requestData <- buildParams
  result <- lift $ App.signup requestData
  case result of
    Left App.UserAlreadyExists -> status status409 >> text "User already exists"
    Right value -> buildResponse value >>= json
