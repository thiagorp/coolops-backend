module Slack.UseCases.CreateProjectIntegration
  ( module Model
  , Params(..)
  , call
  ) where

import RIO

import Model
import Slack.Database.ProjectIntegration

data Params = Params
  { paramChannelId :: !Text
  , paramChannelName :: !Text
  , paramProjectId :: !ProjectId
  }

build :: (MonadIO m) => Params -> m SlackProjectIntegration
build Params {..} = do
  now <- liftIO getCurrentTime
  let slackProjectIntegrationChannelId = paramChannelId
  let slackProjectIntegrationChannelName = paramChannelName
  let slackProjectIntegrationProjectId = paramProjectId
  let slackProjectIntegrationCreatedAt = now
  let slackProjectIntegrationUpdatedAt = now
  return SlackProjectIntegration {..}

create :: (MonadIO m) => Params -> Db m ()
create params = do
  entity <- build params
  void $ insert entity

update_ :: (MonadIO m) => Params -> Entity SlackProjectIntegration -> Db m ()
update_ Params {..} (Entity integrationId _) =
  update
    integrationId
    [SlackProjectIntegrationChannelId =. paramChannelId, SlackProjectIntegrationChannelName =. paramChannelName]

call :: (HasDb m) => Params -> m ()
call params@Params {..} =
  runDb $ do
    maybeIntegration <- findByProjectId paramProjectId
    case maybeIntegration of
      Nothing -> create params
      Just integration -> update_ params integration
