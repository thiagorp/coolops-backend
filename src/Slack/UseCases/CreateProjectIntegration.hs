module Slack.UseCases.CreateProjectIntegration
  ( Params(..)
  , ProjectID
  , call
  ) where

import RIO

import qualified Slack.Database.ProjectIntegration as DB
import Slack.Domain.ProjectIntegration as PI

data Params = Params
  { integrationChannelId :: !Text
  , integrationChannelName :: !Text
  , integrationProjectId :: !ProjectID
  }

build :: (MonadIO m) => Params -> m ProjectIntegration
build Params {..} = do
  integrationId <- genId
  return ProjectIntegration {..}

create :: (DB.HasPostgres m) => Params -> m ProjectIntegration
create params = do
  entity <- build params
  DB.create entity
  return entity

update :: (DB.HasPostgres m) => Params -> ProjectIntegration -> m ProjectIntegration
update Params {..} integation = do
  DB.update newIntegration
  return newIntegration
  where
    newIntegration =
      integation {PI.integrationChannelId = integrationChannelId, PI.integrationChannelName = integrationChannelName}

call :: (DB.HasPostgres m) => Params -> m ProjectIntegration
call params@Params {..} = do
  maybeIntegration <- DB.findByProjectId integrationProjectId
  case maybeIntegration of
    Nothing -> create params
    Just integration -> update params integration
