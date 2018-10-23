module Api.Handlers.Signup
  ( postSignupR
  ) where

import Api.Import

import Auth.Domain
import qualified Auth.UseCases.Signup as App

data Request = Request
  { reqFirstName :: !UserName
  , reqLastName :: !UserName
  , reqEmail :: !UserEmail
  , reqPassword :: !RawPassword
  , reqCompanyName :: !CompanyName
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

mapRequest :: Request -> App.Params
mapRequest Request {..} = App.Params reqFirstName reqLastName reqEmail reqPassword reqCompanyName

buildResponse :: (User, Company) -> Handler Response
buildResponse (User {..}, Company {..}) = do
  resUserToken <- accessTokenTextM userAccessToken
  resCompanyToken <- accessTokenTextM companyToken
  let resUserId = keyText userId
  let resCompanyId = keyText companyId
  return Response {..}

postSignupR :: Handler Value
postSignupR = do
  requestData <- mapRequest <$> requireJsonBody
  result <- App.signup requestData
  case result of
    Left App.UserAlreadyExists -> sendResponseStatus status409 ("User already exists" :: Text)
    Right value -> toJSON <$> buildResponse value
