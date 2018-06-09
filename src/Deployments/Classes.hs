module Deployments.Classes where

import RIO

import Deployments.Domain

class ProjectRepo m where
  createProject :: Project -> m ()
  getProject :: CompanyID -> Text -> m (Maybe Project)
  listProjects :: CompanyID -> m [Project]
