module BackgroundJobs.Handlers.NotifyBuild
  ( Params(..)
  , CallConstraint
  , call
  ) where

import RIO

import Data.Aeson

import Auth.Domain (CompanyID)
import BackgroundJobs.Runner
import qualified Slack.UseCases.NotifyNewBuild as App

type CallConstraint m = (App.CallConstraint m, DbMonad m)

data Params =
  Params CompanyID
         Text

instance FromJSON Params where
  parseJSON =
    withObject "" $ \o -> Params <$> o .: "company_id" <*> o .: "build_id"

instance ToJSON Params where
  toJSON (Params cId bId) = object ["company_id" .= cId, "build_id" .= bId]

call :: App.CallConstraint m => Params -> m JobReturnType
call (Params cId bId) = do
  r <- App.call cId bId
  case r of
    Left App.ProjectNotFound -> finishWithFailure
    Left App.BuildNotFound -> finishWithFailure
    Right _ -> finishWithSuccess
