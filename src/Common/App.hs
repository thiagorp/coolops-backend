module Common.App where

import RIO

import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Client as Http

import Auth.Classes
import Deployments.Classes
import qualified Http.Classes as Http
import qualified Kubernetes.Classes as Kubernetes
import Slack.Api.Classes
import Slack.Classes

import qualified Auth.Database as DB
import qualified Common.Database as DB
import qualified Deployments.Database.Build as DB
import qualified Deployments.Database.Deployment as DB
import qualified Deployments.Database.Environment as DB
import qualified Deployments.Database.Project as DB
import qualified Slack.Database.Team as DB

import qualified Common.Config as Config

data Env = Env
  { pgConn :: PG.Connection
  , requestManager :: Http.Manager
  , kubernetesSettings :: Config.KubernetesSettings
  , slackSettings :: Config.SlackSettings
  }

buildEnv :: PG.Connection -> Http.Manager -> IO Env
buildEnv conn requestManager = do
  k8sSettings <- Config.kubernetesSettings
  slackSettings <- Config.slackSettings
  return $ Env conn requestManager k8sSettings slackSettings

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
  getCompany = DB.getCompany

instance ProjectRepo AppT where
  createProject = DB.createProject
  updateProject = DB.updateProject
  listProjects = DB.listProjects
  getProject = DB.getProject
  getProjectForBuild = DB.getProjectForBuild
  findProjectByAccessToken = DB.findProjectByAccessToken

instance EnvironmentRepo AppT where
  createEnvironment = DB.createEnvironment
  getEnvironment = DB.getEnvironment
  listEnvironments = DB.listEnvironments

instance BuildRepo AppT where
  createBuild = DB.createBuild
  getBuild = DB.getBuild

instance DeploymentRepo AppT where
  createQueuedDeployment = DB.createQueuedDeployment
  getNextQueuedDeployment = DB.getNextQueuedDeployment
  saveRunningDeployment = DB.saveRunningDeployment
  saveFinishedDeployment = DB.saveFinishedDeployment

instance DB.HasDBTransaction AppT where
  runTransaction tx = ask >>= DB.runTransaction_ . run tx
  runEitherTransaction tx = ask >>= DB.runEitherTransaction_ . run tx

instance Http.HasHttpRequestManager AppT where
  getHttpRequestManager = asks requestManager

instance Http.HasHttp AppT

instance Kubernetes.HasKubernetesSettings AppT where
  k8sHost = Config.k8sHost <$> asks kubernetesSettings
  k8sToken = Config.k8sToken <$> asks kubernetesSettings
  k8sNamespace = Config.k8sNamespace <$> asks kubernetesSettings

instance SlackTeamRepo AppT where
  createSlackTeam = DB.createSlackTeam
  getSlackTeam = DB.getSlackTeam
  deleteSlackTeam = DB.deleteSlackTeam

instance HasSlackSettings AppT where
  slackClientId = Config.slackClientId <$> asks slackSettings
  slackClientSecret = Config.slackClientSecret <$> asks slackSettings
