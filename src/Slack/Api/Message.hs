module Slack.Api.Message where

import RIO

import Data.Aeson

data Message = Message
  { messageText :: !(Maybe Text)
  , messageAttachments :: !(Maybe [Attachment])
  }

data Attachment = Attachment
  { attachmentTitle :: !(Maybe Text)
  , attachmentPretext :: !(Maybe Text)
  , attachmentText :: !(Maybe Text)
  , attachmentCallbackId :: !(Maybe Text)
  , attachmentType :: !(Maybe Text)
  , attachmentActions :: !(Maybe [Action])
  , attachmentFooter :: !(Maybe Text)
  , attachmentColor :: !(Maybe Text)
  , attachmentMarkdown :: !(Maybe [Text])
  }

data Action = Action
  { actionName :: !Text
  , actionText :: !Text
  , actionType :: !Text
  , actionValue :: !Text
  }

slackMessage :: Message
slackMessage = Message Nothing Nothing

slackAttachment :: Attachment
slackAttachment =
  Attachment
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

slackAction :: Action
slackAction =
  Action {actionName = "", actionText = "", actionType = "", actionValue = ""}

instance ToJSON Message where
  toJSON Message {..} =
    object ["text" .= messageText, "attachments" .= messageAttachments]

instance ToJSON Attachment where
  toJSON Attachment {..} =
    object
      [ "title" .= attachmentTitle
      , "pretext" .= attachmentPretext
      , "text" .= attachmentText
      , "callback_id" .= attachmentCallbackId
      , "type" .= attachmentType
      , "actions" .= attachmentActions
      , "footer" .= attachmentFooter
      , "color" .= attachmentColor
      , "mrkdwn_in" .= attachmentMarkdown
      ]

instance ToJSON Action where
  toJSON Action {..} =
    object
      [ "name" .= actionName
      , "text" .= actionText
      , "type" .= actionType
      , "value" .= actionValue
      ]
