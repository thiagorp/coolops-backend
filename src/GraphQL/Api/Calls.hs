{-# LANGUAGE RecordWildCards #-}

module GraphQL.Api.Calls
  ( module GraphQL.Api.Types
  , listSlackConversations
  ) where

import Import

import GraphQL.Api.Types
import qualified Slack.Api.Conversations as Api

listSlackConversations :: [Text] -> App [(Text, [SlackChannel])]
listSlackConversations = traverse listSlackConversationsForToken

listSlackConversationsForToken :: Text -> App (Text, [SlackChannel])
listSlackConversationsForToken token = (,) token . map mapSlackConversation . Api.conversations <$> Api.list token

mapSlackConversation :: Api.Conversation -> SlackChannel
mapSlackConversation Api.Conversation {..} =
  let slackChannelId = conversationId
      slackChannelName = conversationName
   in SlackChannel {..}
