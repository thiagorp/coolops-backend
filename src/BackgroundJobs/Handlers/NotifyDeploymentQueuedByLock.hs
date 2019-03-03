{-# LANGUAGE OverloadedStrings #-}

module BackgroundJobs.Handlers.NotifyDeploymentQueuedByLock
  ( Params(..)
  , call
  ) where

import Import

import Data.Aeson

import BackgroundJobs.Runner
import qualified Slack.UseCases.NotifyDeploymentQueuedByLock as App (call)

data Params = Params DeploymentId EnvironmentLockId Text

instance FromJSON Params where
  parseJSON = withObject "" $ \o ->
    Params <$>  o .: "deployment_id" <*> o .: "lock_id" <*> o .: "to_user_id"

instance ToJSON Params where
  toJSON (Params dId elId tuId) =
    object
      [ "deployment_id" .= dId
      , "lock_id" .= elId
      , "to_user_id" .= tuId
      ]

call :: Params -> App JobReturnType
call (Params dId elId tuId) = do
  App.call dId elId tuId
  finishWithSuccess
