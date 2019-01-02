module BackgroundJobs.Handlers.NotifyNewEnvironmentLock
  ( Params(..)
  , call
  ) where

import Import

import Data.Aeson

import BackgroundJobs.Runner
import Slack.UseCases.NotifyNewEnvironmentLock hiding (call)
import qualified Slack.UseCases.NotifyNewEnvironmentLock as App (call)

data Params = Params CompanyId EnvironmentLockId

instance FromJSON Params where
  parseJSON = withObject "" $ \o -> Params <$> o .: "company_id" <*> o .: "environment_lock_id"

instance ToJSON Params where
  toJSON (Params cId lId) = object ["company_id" .= cId, "environment_lock_id" .= lId]

call :: Params -> App JobReturnType
call (Params cId lId) = do
  r <- App.call cId lId
  case r of
    Left MessageDataNotFound -> finishWithFailure "Message data not found"
    Right _ -> finishWithSuccess
