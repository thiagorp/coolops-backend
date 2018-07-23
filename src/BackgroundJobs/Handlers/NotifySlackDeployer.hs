module BackgroundJobs.Handlers.NotifySlackDeployer
  ( Params(..)
  , CallConstraint
  , call
  ) where

import RIO

import Data.Aeson

import BackgroundJobs.Runner
import Deployments.Domain.Project (CompanyID)
import qualified Slack.UseCases.NotifyUserOfFinishedDeployment as App

type CallConstraint m = (App.CallConstraint m, DbMonad m)

data Params =
  Params CompanyID
         Text

instance FromJSON Params where
  parseJSON =
    withObject "" $ \o -> Params <$> o .: "company_id" <*> o .: "deployment_id"

instance ToJSON Params where
  toJSON (Params cId dId) = object ["company_id" .= cId, "deployment_id" .= dId]

call :: CallConstraint m => Params -> m JobReturnType
call (Params cId dId) = do
  r <- App.call cId dId
  case r of
    Left App.DataNotFound -> finishWithFailure "Data not found"
    Right _ -> finishWithSuccess
