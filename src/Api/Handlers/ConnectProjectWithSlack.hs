module Api.Handlers.ConnectProjectWithSlack
  ( postProjectsSlackIntegrationR
  ) where

import Api.Import

import qualified Slack.UseCases.IntegrateProjectFromOAuth as App

data Fields =
  Code

instance HasFieldName Fields where
  fieldName field =
    case field of
      Code -> "code"

newtype Request = Request
  { reqCode :: Maybe Text
  }

instance FromJSON Request where
  parseJSON =
    withObject "request params" $ \o -> do
      reqCode <- o .:? fieldName Code
      return Request {..}

builder :: User -> Text -> Request -> WebValidation App.Params
builder User {..} projectId Request {..} = App.Params <$> pure userCompanyId <*> pure projectId <*> code
  where
    code = required_ Code reqCode |>> valid

call :: Text -> AuthenticatedUser -> Handler ()
call projectId (AuthenticatedUser user) = do
  appParams <- requireJsonBody >>= parseValidatedRequest (builder user projectId)
  result <- App.call appParams
  case result of
    Right _ -> sendResponseStatus created201 ()
    Left App.ProjectNotFound -> notFound
    Left App.CouldNotExchangeCode -> sendResponseStatus internalServerError500 ()

postProjectsSlackIntegrationR :: Text -> Handler ()
postProjectsSlackIntegrationR projectId = userAuth (call projectId)
