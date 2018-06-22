module Common.App where

import RIO

import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Client as Http

import Auth.Classes
import Deployments.Classes

import qualified Http.Classes as Http

import qualified Auth.Database as DB
import qualified Common.Database as DB

import qualified Deployments.Database.Build as DB
import qualified Deployments.Database.Deployment as DB
import qualified Deployments.Database.Environment as DB
import qualified Deployments.Database.Project as DB

data Env = Env
  { pgConn :: PG.Connection
  , requestManager :: Http.Manager
  }

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader Env)

run :: AppT a -> Env -> IO a
run app = runReaderT (unAppT app)

instance DB.HasPostgresConnection AppT where
  getPostgresConn = asks pgConn

instance DB.HasPostgres AppT

instance UserRepo AppT where
  createUser = DB.createUser
  findUserByEmail = DB.findUserByEmail
  findUserByAccessToken = DB.findUserByAccessToken

instance CompanyRepo AppT where
  createCompany = DB.createCompany
  listCompanies = DB.listCompanies

instance ProjectRepo AppT where
  createProject = DB.createProject
  updateProject = DB.updateProject
  listProjects = DB.listProjects
  getProject = DB.getProject
  findProjectByAccessToken = DB.findProjectByAccessToken

instance EnvironmentRepo AppT where
  createEnvironment = DB.createEnvironment
  getEnvironment = DB.getEnvironment

instance BuildRepo AppT where
  createBuild = DB.createBuild
  getBuild = DB.getBuild

instance DeploymentRepo AppT where
  createQueuedDeployment = DB.createQueuedDeployment
  getNextQueuedDeployment = DB.getNextQueuedDeployment
  saveRunningDeployment = DB.saveRunningDeployment

instance DB.HasDBTransaction AppT where
  runTransaction tx = ask >>= DB.runTransaction_ . run tx
  runEitherTransaction tx = ask >>= DB.runEitherTransaction_ . run tx

instance Http.HasHttpRequestManager AppT where
  getHttpRequestManager = asks requestManager

instance Http.HasHttp AppT
