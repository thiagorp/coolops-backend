module Deployments.UseCases.CreateOnboardingProject
  ( Params(..)
  , call
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Common.Database
import Deployments.Classes (getProject)
import Deployments.Database.Onboarding
import qualified Deployments.Domain.Environment as Environment
import Deployments.Domain.Onboarding
import qualified Deployments.Domain.Project as Project
import qualified Deployments.UseCases.CreateEnvironment as CreateEnvironment
import qualified Deployments.UseCases.CreateProject as CreateProject
import qualified Deployments.UseCases.UpdateProject as UpdateProject
import Util.Validation

data Params = Params
  { projectName :: !CreateProject.ProjectName
  , companyId :: !CompanyID
  }

buildProjectParams :: (MonadIO m) => Params -> m CreateProject.Params
buildProjectParams Params {..} = do
  projectDeploymentImage <-
    case Project.buildDeploymentImage "coolopsio/onboarding:1" of
      Valid image -> return image
      Invalid _ -> fail "The deployment image when creating a project on onboarding should never be invalid"
  return CreateProject.Params {..}

buildCreateEnvParams :: (MonadIO m) => Text -> Text -> m CreateEnvironment.Params
buildCreateEnvParams envName appName = do
  let environmentEnvVars = HashMap.fromList [("APP_NAME", appName)]
  environmentName <-
    case Environment.buildName envName of
      Valid name -> return name
      Invalid _ -> fail "The environment name when creating an environment on onboarding should never be invalid"
  return CreateEnvironment.Params {..}

createProject ::
     (MonadIO m, CreateProject.ProjectRepo m, CreateEnvironment.EnvironmentRepo m) => Params -> m CreateProject.Project
createProject params = do
  createProjectParams <- buildProjectParams params
  project <- CreateProject.call createProjectParams
  _ <- buildCreateEnvParams "Staging" "Staging Onboarding App" >>= CreateEnvironment.call_ project
  _ <- buildCreateEnvParams "Production" "Production Onboarding App" >>= CreateEnvironment.call_ project
  return project

updateProject ::
     (MonadIO m, UpdateProject.ProjectRepo m, CreateEnvironment.EnvironmentRepo m)
  => ProjectID
  -> Params
  -> m UpdateProject.Project
updateProject pId params@Params {..} = do
  maybeProject <- getProject companyId pId
  case maybeProject of
    Nothing -> createProject params
    Just project ->
      UpdateProject.call project $ UpdateProject.Params projectName (Project.projectDeploymentImage project)

upsertProject ::
     (MonadIO m, CreateProject.ProjectRepo m, CreateEnvironment.EnvironmentRepo m)
  => Onboarding
  -> Params
  -> m CreateProject.Project
upsertProject Onboarding {..} params@Params {..} =
  case onboardingProjectId of
    Nothing -> createProject params
    Just pId -> updateProject pId params

call ::
     (HasDBTransaction m, HasPostgres m, CreateProject.ProjectRepo m, CreateEnvironment.EnvironmentRepo m)
  => Params
  -> m (CreateProject.Project, Onboarding)
call params = do
  onboarding <- getOnboarding (companyId params)
  runTransaction $ do
    project <- upsertProject onboarding params
    upsertOnboarding onboarding {onboardingProjectId = Just (Project.projectId project)}
    return (project, onboarding)
