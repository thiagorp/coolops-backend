module Api.Handlers.SlackMessageButtons.ReleaseEnvironmentLock
  ( call
  ) where

import Api.Import

import qualified Deployments.UseCases.ReleaseEnvironmentLock as App

call :: Text -> EnvironmentLockId -> Handler (Maybe a)
call userId lockId = do
  runAppInHandler $ App.call userId lockId
  return Nothing
