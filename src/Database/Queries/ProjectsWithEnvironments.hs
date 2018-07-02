module Database.Queries.ProjectsWithEnvironments
  ( Build(..)
  , Deployment(..)
  , Environment(..)
  , Project(..)
  , DbStatus(..)
  , listProjectsWithEnvironments
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Common.Database
import Deployments.Database.Deployment (DbStatus(..))
import Deployments.Domain.Project (CompanyID)

data Build = Build
  { buildId :: !UUID
  , buildName :: !Text
  }

data Deployment = Deployment
  { deploymentId :: !UUID
  , deploymentLastChangedAt :: !UTCTime
  , deploymentStatus :: !DbStatus
  , deploymentBuild :: !Build
  }

data Environment = Environment
  { environmentId :: !UUID
  , environmentName :: !Text
  , environmentCurrentDeployment :: !(Maybe Deployment)
  }

data Project = Project
  { projectId :: !UUID
  , projectName :: !Text
  , projectDeploymentImage :: !Text
  , projectAccessToken :: !Text
  , projectEnvironments :: ![Environment]
  }

instance FromRow Project where
  fromRow = do
    projectId <- field
    projectName <- field
    projectDeploymentImage <- field
    projectAccessToken <- field
    mEnvironmentId <- field
    mEnvironmentName <- field
    mDeploymentId <- field
    mDeploymentStatus <- field
    mDeploymentLastChangedAt <- field
    mBuildId <- field
    mBuildName <- field
    let mBuild = Build <$> mBuildId <*> mBuildName
    let mDeployment =
          Deployment <$> mDeploymentId <*>
          (localTimeToUTC utc <$> mDeploymentLastChangedAt) <*>
          mDeploymentStatus <*>
          mBuild
    let mEnvironment =
          Environment <$> mEnvironmentId <*> mEnvironmentName <*>
          pure mDeployment
    let projectEnvironments =
          case mEnvironment of
            Nothing -> []
            Just environment -> [environment]
    return Project {..}

flattenProjects ::
     Project -> HashMap.HashMap UUID Project -> HashMap.HashMap UUID Project
flattenProjects project hash =
  let key = projectId project
      environmentToAdd =
        case HashMap.lookup key hash of
          Nothing -> []
          Just existingProject -> projectEnvironments existingProject
      newProject =
        project
          { projectEnvironments =
              (projectEnvironments project) <> environmentToAdd
          }
   in HashMap.insert key newProject hash

listProjectsWithEnvironments :: HasPostgres m => CompanyID -> m [Project]
listProjectsWithEnvironments companyId = do
  projects <- runQuery q (Only companyId)
  let flattenedProjects = foldr flattenProjects HashMap.empty projects
  return $ HashMap.elems flattenedProjects
  where
    q =
      "select distinct on (p.id, e.id)\
        \ p.id, p.name, p.deployment_image, p.access_token,\
        \ e.id, e.name,\
        \ d.id, d.status, d.updated_at,\
        \ b.id, b.name\
        \ from projects p\ 
        \ left join environments e on e.project_id = p.id\
        \ left join deployments d on d.environment_id = e.id\
        \ left join builds b on b.id = d.build_id\
        \ where p.company_id = ?\
        \ order by p.id, e.id, d.updated_at desc"
