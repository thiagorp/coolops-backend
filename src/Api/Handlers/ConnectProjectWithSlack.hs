module Api.Handlers.ConnectProjectWithSlack
  ( postProjectsSlackIntegrationR
  ) where

import Api.Import

import qualified Slack.UseCases.IntegrateProjectFromOAuth as App

newtype Request = Request
  { reqCode :: Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqCode <- o .: "code"
      return Request {..}

mapRequest :: User -> Text -> Request -> App.Params
mapRequest User {..} projectId Request {..} = App.Params userCompanyId projectId reqCode

call :: Text -> AuthenticatedUser -> Handler ()
call projectId (AuthenticatedUser user) = do
  appParams <- mapRequest user projectId <$> requireJsonBody
  result <- App.call appParams
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left App.ProjectNotFound -> notFound
    Left App.CouldNotExchangeCode -> sendResponseStatus internalServerError500 ()

postProjectsSlackIntegrationR :: Text -> Handler ()
postProjectsSlackIntegrationR projectId = userAuth (call projectId)
