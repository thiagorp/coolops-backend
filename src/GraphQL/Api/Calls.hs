module GraphQL.Api.Calls
  ( module GraphQL.Api.Types
  , listSlackConversations
  ) where

import RIO

import GraphQL.Api.Types
import qualified Slack.Api.Conversations as Api

listSlackConversations :: (Api.SlackClientMonad m) => [Text] -> m [(Text, [SlackChannel])]
listSlackConversations = traverse listSlackConversationsForToken

listSlackConversationsForToken :: (Api.SlackClientMonad m) => Text -> m (Text, [SlackChannel])
listSlackConversationsForToken token = (,) token . map mapSlackConversation . Api.conversations <$> Api.list token

mapSlackConversation :: Api.Conversation -> SlackChannel
mapSlackConversation Api.Conversation {..} =
  let slackChannelId = conversationId
      slackChannelName = conversationName
   in SlackChannel {..}
