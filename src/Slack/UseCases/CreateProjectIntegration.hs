module Slack.UseCases.CreateProjectIntegration
  ( module Model
  , Params(..)
  , call
  ) where

import Import

import Model
import Slack.Database.ProjectIntegration

data Params = Params
  { paramChannelId :: !Text
  , paramChannelName :: !Text
  , paramProjectId :: !ProjectId
  }

build :: Params -> App SlackProjectIntegration
build Params {..} = do
  now <- liftIO getCurrentTime
  let slackProjectIntegrationChannelId = paramChannelId
  let slackProjectIntegrationChannelName = paramChannelName
  let slackProjectIntegrationProjectId = paramProjectId
  let slackProjectIntegrationCreatedAt = now
  let slackProjectIntegrationUpdatedAt = now
  return SlackProjectIntegration {..}

create :: Params -> App ()
create params = do
  entity <- build params
  void $ insert entity

update_ :: Params -> Entity SlackProjectIntegration -> App ()
update_ Params {..} (Entity integrationId _) =
  update
    integrationId
    [SlackProjectIntegrationChannelId =. paramChannelId, SlackProjectIntegrationChannelName =. paramChannelName]

call :: Params -> App ()
call params@Params {..} = do
  maybeIntegration <- findByProjectId paramProjectId
  case maybeIntegration of
    Nothing -> create params
    Just integration -> update_ params integration
