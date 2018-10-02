module Deployments.UseCases.CreateBuild
  ( Params(..)
  , call
  ) where

import RIO

import qualified BackgroundJobs.AppJobs as Background
import Deployments.Database.Build (createBuild)
import qualified Deployments.Domain.Build as Build
import qualified Deployments.Domain.Project as Project
import Util.Key

data Params = Params
  { buildName :: !Build.Name
  , buildParams :: !(HashMap Text Text)
  , buildMetadata :: !(HashMap Text Text)
  , buildProject :: !Project.Project
  }

entity :: (MonadIO m) => Params -> m Build.Build
entity Params {..} = do
  buildId <- Build.genId
  let buildProjectId = Project.projectId buildProject
  return Build.Build {..}

notify :: (Background.NotifyBuildConstraint m) => Project.Project -> Build.Build -> m ()
notify Project.Project {..} Build.Build {..} = Background.notifyBuild projectCompanyId (keyText buildId)

call :: (MonadIO m, Background.NotifyBuildConstraint m) => Params -> m Build.Build
call params@Params {..} = do
  build <- entity params
  createBuild build
  notify buildProject build
  return build
