module Api.Handlers.Signup
  ( postSignupR
  ) where

import Api.Import

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
builder Request {..} = App.Params <$> firstName <*> lastName <*> email <*> password <*> companyName
  where
    password = required_ Password reqPassword |>> buildPassword
    firstName = required_ FirstName reqFirstName |>> buildUserName
    lastName = required_ LastName reqLastName |>> buildUserName
    email = required_ Email reqEmail |>> buildEmailAddress
    companyName = required_ CompanyName reqCompanyName |>> buildCompanyName

buildResponse :: (User, Company) -> Handler Response
buildResponse (User {..}, Company {..}) = do
  resUserToken <- accessTokenTextM userAccessToken
  resCompanyToken <- accessTokenTextM companyToken
  let resUserId = keyText userId
  let resCompanyId = keyText companyId
  return Response {..}

postSignupR :: Handler Value
postSignupR = do
  requestData <- requireJsonBody >>= parseValidatedRequest builder
  result <- App.signup requestData
  case result of
    Left App.UserAlreadyExists -> sendResponseStatus status409 ("User already exists" :: Text)
    Right value -> toJSON <$> buildResponse value
