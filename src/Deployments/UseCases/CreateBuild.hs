module Deployments.UseCases.CreateBuild
  ( Params(..)
  , call
  ) where

import RIO

import Deployments.Classes
import Deployments.Domain

data Params = Params
  { buildName :: !BuildName
  , buildParams :: !(HashMap Text Text)
  , buildProject :: !Project
  }

entity :: (MonadIO m) => Params -> m Build
entity Params {..} = do
  buildId <- genBuildId
  let buildProjectId = projectId buildProject
  return Build {..}

call :: (MonadIO m, BuildRepo m) => Params -> m Build
call params = do
  build <- entity params
  createBuild build
  return build
