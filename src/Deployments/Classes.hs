module Deployments.Classes where

import RIO

import Deployments.Domain

class (Monad m) =>
      ProjectRepo m
  where
  createProject :: Project -> m ()
  updateProject :: Project -> m ()
  getProject :: CompanyID -> Text -> m (Maybe Project)
  listProjects :: CompanyID -> m [Project]
