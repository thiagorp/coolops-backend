module Deployments.UseCases.CreateBuild
  ( Params(..)
  , call
  ) where

import RIO

import Deployments.Classes
import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Project as Project

data Params = Params
  { buildName :: !Build.Name
  , buildParams :: !(HashMap Text Text)
  , buildProject :: !Project.Project
  }

entity :: (MonadIO m) => Params -> m Build.Build
entity Params {..} = do
  buildId <- Build.genId
  let buildProjectId = Project.projectId buildProject
  return Build.Build {..}

call :: (MonadIO m, BuildRepo m) => Params -> m Build.Build
call params = do
  build <- entity params
  createBuild build
  return build