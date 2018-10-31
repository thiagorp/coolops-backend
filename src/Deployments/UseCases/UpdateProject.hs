module Deployments.UseCases.UpdateProject
  ( module Model
  , Params(..)
  , call
  ) where

import RIO

import Deployments.Database.Project
import Model

data Params = Params
  { paramProjectName :: !ProjectName
  , paramProjectSlug :: !Slug
  , paramProjectDeploymentImage :: !DockerImage
  }

call :: (HasDb m) => ProjectId -> Params -> m ()
call projectId Params {..} = do
  now <- liftIO getCurrentTime
  runDb $
    update
      projectId
      [ ProjectName =. paramProjectName
      , ProjectSlug =. paramProjectSlug
      , ProjectDeploymentImage =. paramProjectDeploymentImage
      , ProjectUpdatedAt =. now
      ]
