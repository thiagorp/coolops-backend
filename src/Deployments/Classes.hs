module Deployments.Classes where

import Deployments.Domain

class ProjectRepo m where
  createProject :: Project -> m ()
  listProjects :: CompanyID -> m [Project]
