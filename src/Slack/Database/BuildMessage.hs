module Slack.Database.BuildMessage
  ( getSlackBuildMessage
  ) where

import RIO hiding ((^.))

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

getSlackBuildMessage :: (MonadIO m) => BuildId -> Db m (Maybe (Entity SlackBuildMessage))
getSlackBuildMessage buildId =
  selectFirst $
  from $ \b -> do
    where_ (b ^. SlackBuildMessageBuildId ==. val buildId)
    return b
