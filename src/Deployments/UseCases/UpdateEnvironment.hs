module Deployments.UseCases.UpdateEnvironment
  ( module Model
  , Params(..)
  , call
  ) where

import RIO

import Deployments.Database.Environment
import Model

data Params = Params
  { paramEnvironmentName :: !EnvironmentName
  , paramEnvironmentSlug :: !Slug
  , paramEnvironmentEnvVars :: !(HashMap Text Text)
  }

call :: (HasDb m) => EnvironmentId -> Params -> m ()
call eId Params {..} = do
  now <- liftIO getCurrentTime
  runDb $
    update
      eId
      [ EnvironmentName =. paramEnvironmentName
      , EnvironmentEnvVars =. paramEnvironmentEnvVars
      , EnvironmentSlug =. paramEnvironmentSlug
      , EnvironmentUpdatedAt =. now
      ]
