{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Handlers.CreateSlackAccessToken
  ( postCreateSlackAccessTokenR
  ) where

import Api.Import

import qualified Slack.UseCases.CreateAccessToken as App

newtype Request = Request
  { reqCode :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqCode <- o .: "code"
      return Request {..}

mapRequest :: User -> Request -> App.Params
mapRequest User {..} Request {..} = App.Params userCompanyId reqCode

call :: Entity User -> Handler ()
call (Entity _ user) = do
  appParams <- mapRequest user <$> requireJsonBody
  result <- runAppInHandler $ App.call appParams
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left App.CouldNotExchangeCode -> sendResponseStatus internalServerError500 ()

postCreateSlackAccessTokenR :: Handler ()
postCreateSlackAccessTokenR = userAuth call
