module Deployments.UseCases.UpdateEnvironment where

import RIO

import Common.Database
import Deployments.Database.Environment (updateEnvironment)
import qualified Deployments.Domain.Environment as Environment

data Params = Params
  { paramEnvironmentName :: !Environment.Name
  , paramEnvironmentSlug :: !Environment.Slug
  , paramEnvironmentEnvVars :: !(HashMap Text Text)
  }

apply :: Environment.Environment -> Params -> Environment.Environment
apply project Params {..} =
  project
    { Environment.environmentName = paramEnvironmentName
    , Environment.environmentEnvVars = paramEnvironmentEnvVars
    , Environment.environmentSlug = paramEnvironmentSlug
    }

call :: (HasPostgres m) => Environment.Environment -> Params -> m Environment.Environment
call environment params = do
  let updatedEnvironment = apply environment params
  updateEnvironment updatedEnvironment
  return updatedEnvironment
