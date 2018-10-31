module Deployments.UseCases.CreateBuild
  ( module Common.PersistDatabase
  , module Model
  , Params(..)
  , call
  ) where

import RIO

import qualified BackgroundJobs.AppJobs as Background
import Common.PersistDatabase
import Model

data Params = Params
  { paramName :: !BuildName
  , paramParams :: !(HashMap Text Text)
  , paramMetadata :: !(HashMap Text Text)
  , paramProject :: !(Entity Project)
  }

entity :: (MonadIO m) => Params -> Db m Build
entity Params {..} = do
  now <- liftIO getCurrentTime
  let buildName = paramName
  let buildProjectId = entityKey paramProject
  let buildParams = paramParams
  let buildMetadata = paramMetadata
  let buildCreatedAt = now
  let buildUpdatedAt = now
  return Build {..}

notify :: (Background.NotifyBuildConstraint m) => Project -> Key Build -> Db m ()
notify Project {..} (BuildKey buildId) = void $ Background.notifyBuild projectCompanyId buildId

call :: (MonadIO m, Background.NotifyBuildConstraint m) => Params -> Db m Build
call params@Params {..} = do
  build <- entity params
  buildId <- insert build
  notify (entityVal paramProject) buildId
  return build
