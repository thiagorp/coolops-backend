module GraphQL.Api.Types where

import RIO

data SlackChannel = SlackChannel
  { slackChannelId :: Text
  , slackChannelName :: Text
  } deriving (Show)
