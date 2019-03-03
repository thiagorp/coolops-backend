{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handlers.Signup
  ( postSignupR
  ) where

import Api.Import

import qualified Auth.UseCases.Signup as App

data Request = Request
  { reqFirstName :: !UserName
  , reqLastName :: !UserName
  , reqEmail :: !EmailAddress
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
  { resUserToken :: !AccessToken
  , resCompanyToken :: !AccessToken
  , resUserId :: !UserId
  , resCompanyId :: !CompanyId
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

buildResponse :: (Entity User, Entity Company) -> Handler Response
buildResponse (Entity userId User {..}, Entity companyId Company {..}) = do
  let resUserToken = userAccessToken
  let resCompanyToken = companyAccessToken
  let resUserId = userId
  let resCompanyId = companyId
  return Response {..}

postSignupR :: Handler Value
postSignupR = do
  requestData <- mapRequest <$> requireCheckJsonBody
  result <- runAppInHandler $ App.signup requestData
  case result of
    Left App.UserAlreadyExists -> sendResponseStatus status409 ("User already exists" :: Text)
    Right value -> toJSON <$> buildResponse value
