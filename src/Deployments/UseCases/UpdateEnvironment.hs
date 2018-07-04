module Deployments.UseCases.UpdateEnvironment where

import RIO

import Deployments.Classes (EnvironmentRepo, updateEnvironment)
import qualified Deployments.Domain.Environment as Environment

data Params = Params
  { paramEnvironmentName :: !Environment.Name
  , paramEnvironmentEnvVars :: !(HashMap Text Text)
  }

apply :: Environment.Environment -> Params -> Environment.Environment
apply project (Params {..}) = do
  project
    { Environment.environmentName = paramEnvironmentName
    , Environment.environmentEnvVars = paramEnvironmentEnvVars
    }

call ::
     (EnvironmentRepo m)
  => Environment.Environment
  -> Params
  -> m Environment.Environment
call environment params = do
  let updatedEnvironment = apply environment params
  updateEnvironment updatedEnvironment
  return updatedEnvironment
