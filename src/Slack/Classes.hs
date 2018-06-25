module Slack.Classes where

import RIO

import Auth.Domain (CompanyID)
import Slack.Domain.Team

class SlackTeamRepo m where
  createSlackTeam :: Team -> m ()
  getSlackTeam :: CompanyID -> m (Maybe Team)
  deleteSlackTeam :: Team -> m ()
