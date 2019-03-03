{-# LANGUAGE RecordWildCards #-}

module Deployments.UseCases.CreateBuild
  ( Params(..)
  , call
  ) where

import Import

import qualified BackgroundJobs.AppJobs as Background

data Params = Params
  { paramName :: !BuildName
  , paramParams :: !(HashMap Text Text)
  , paramMetadata :: !(HashMap Text Text)
  , paramProject :: !(Entity Project)
  }

entity :: Params -> App Build
entity Params {..} = do
  now <- liftIO getCurrentTime
  let buildName = paramName
  let buildProjectId = entityKey paramProject
  let buildParams = paramParams
  let buildMetadata = paramMetadata
  let buildCreatedAt = now
  let buildUpdatedAt = now
  return Build {..}

notify :: Project -> Key Build -> App ()
notify Project {..} (BuildKey buildId) = void $ Background.notifyBuild projectCompanyId buildId

call :: Params -> App Build
call params@Params {..} = do
  build <- entity params
  buildId <- insert build
  notify (entityVal paramProject) buildId
  return build
