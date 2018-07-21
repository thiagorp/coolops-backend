module Slack.Domain.BuildMessage
  ( ID
  , BuildMessage(..)
  , BuildID
  , genId
  ) where

import RIO

import qualified Deployments.Domain.Build as Build
import Util.Key

type ID = Key BuildMessage

type BuildID = Build.ID

data BuildMessage = BuildMessage
  { buildMessageId :: !ID
  , buildMessageBuildId :: !BuildID
  , buildMessageSlackMessageId :: !Text
  }

genId :: MonadIO m => m ID
genId = genID
