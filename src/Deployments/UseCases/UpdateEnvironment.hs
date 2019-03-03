{-# LANGUAGE RecordWildCards #-}

module Deployments.UseCases.UpdateEnvironment
  ( Params(..)
  , call
  ) where

import Import

data Params = Params
  { paramEnvironmentName :: !EnvironmentName
  , paramEnvironmentSlug :: !Slug
  , paramEnvironmentEnvVars :: !(HashMap Text Text)
  }

call :: EnvironmentId -> Params -> App ()
call eId Params {..} = do
  now <- liftIO getCurrentTime
  update
    eId
    [ EnvironmentName =. paramEnvironmentName
    , EnvironmentEnvVars =. paramEnvironmentEnvVars
    , EnvironmentSlug =. paramEnvironmentSlug
    , EnvironmentUpdatedAt =. now
    ]
