module Api.Handlers.Signup
  ( postSignupR
  ) where

import Api.Import

import qualified Auth.UseCases.Signup as App

data Request = Request
  { reqFirstName :: !App.UserName
  , reqLastName :: !App.UserName
  , reqEmail :: !App.EmailAddress
  , reqPassword :: !App.RawPassword
  , reqCompanyName :: !App.CompanyName
  }

instance FromJSON Request where
  parseJSON =
    withObject "signup params" $ \o -> do
      reqFirstName <- o .: "first_name"
      reqLastName <- o .: "last_name"
      reqEmail <- o .: "email"
      reqPassword <- o .: "password"
      reqCompanyName <- o .: "company_name"
      return Request {..}

data Response = Response
  { resUserToken :: !App.AccessToken
  , resCompanyToken :: !App.AccessToken
  , resUserId :: !App.UserId
  , resCompanyId :: !App.CompanyId
  }

instance ToJSON Response where
  toJSON Response {..} =
    object
      [ "user_access_token" .= resUserToken
      , "user_id" .= resUserId
      , "company_access_token" .= resCompanyToken
      , "company_id" .= resCompanyId
      ]

mapRequest :: Request -> App.Params
mapRequest Request {..} = App.Params reqFirstName reqLastName reqEmail reqPassword reqCompanyName

buildResponse :: (App.Entity App.User, App.Entity App.Company) -> Handler Response
buildResponse (App.Entity userId App.User {..}, App.Entity companyId App.Company {..}) = do
  let resUserToken = userAccessToken
  let resCompanyToken = companyAccessToken
  let resUserId = userId
  let resCompanyId = companyId
  return Response {..}

postSignupR :: Handler Value
postSignupR = do
  requestData <- mapRequest <$> requireJsonBody
  result <- App.signup requestData
  case result of
    Left App.UserAlreadyExists -> sendResponseStatus status409 ("User already exists" :: Text)
    Right value -> toJSON <$> buildResponse value
