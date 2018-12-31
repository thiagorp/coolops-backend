module Deployments.UseCases.UpdateProject
  ( Params(..)
  , call
  ) where

import Import

data Params = Params
  { paramProjectName :: !ProjectName
  , paramProjectSlug :: !Slug
  , paramProjectDeploymentImage :: !DockerImage
  }

call :: ProjectId -> Params -> App ()
call projectId Params {..} = do
  now <- liftIO getCurrentTime
  update
    projectId
    [ ProjectName =. paramProjectName
    , ProjectSlug =. paramProjectSlug
    , ProjectDeploymentImage =. paramProjectDeploymentImage
    , ProjectUpdatedAt =. now
    ]
