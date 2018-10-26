module GraphQL.Database
  ( App
  , buildEnv
  , getBuild
  , getCompany
  , getEnvironment
  , getEnvLastDeployment
  , getOnboarding
  , getProject
  , getSlackAccessToken
  , getSlackProjectIntegration
  , getUser
  , listBuilds
  , listEnvironments
  , listProjects
  , listSlackChannels
  , run
  ) where

import RIO
import qualified RIO.List as List

import Data.Hashable
import Data.Pool
import Haxl.Core

import Auth.Domain (User(..))
import qualified Common.Config as Config
import Common.Database hiding (runDb)
import qualified Env as App
import qualified GraphQL.Api.Calls as Api
import qualified GraphQL.Database.Queries as Q
import Http.Classes
import Slack.Api.Classes

type App = GenHaxl AppEnv

data AppEnv = AppEnv
  { currentUser :: !User
  , appEnv :: !App.Env
  }

newtype Db a =
  Db (ReaderT App.Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader App.Env, MonadUnliftIO, MonadThrow)

instance HasPostgres Db where
  getPostgresConn fx = do
    pool <- asks App.pgConnPool
    withRunInIO $ \r -> withResource pool (r . fx)

instance HasHttp Db where
  getHttpRequestManager = asks App.requestManager

instance HasSlackSettings Db where
  slackClientId = Config.slackClientId <$> asks App.slackSettings
  slackClientSecret = Config.slackClientSecret <$> asks App.slackSettings
  slackVerificationToken = Config.slackVerificationToken <$> asks App.slackSettings
  slackSigningSecret = Config.slackSigningSecret <$> asks App.slackSettings

runDb :: App.Env -> Db a -> IO a
runDb e (Db m) = runReaderT m e

-- Util
run :: MonadIO m => Env AppEnv -> App a -> m a
run e a = liftIO $ runHaxl e a

buildEnv :: MonadIO m => User -> App.Env -> m (Env AppEnv)
buildEnv u e = do
  let stateStore = stateSet UserState {} stateEmpty
  liftIO $ initEnv stateStore $ AppEnv u e

-- DB Functions
getBuild :: Q.BuildID -> App (Maybe Q.Build)
getBuild bId = dataFetch (GetBuild bId)

getCompany :: App (Maybe Q.Company)
getCompany = dataFetch GetCompany

getEnvironment :: Q.EnvironmentID -> App (Maybe Q.Environment)
getEnvironment eId = dataFetch (GetEnvironment eId)

getEnvLastDeployment :: Q.EnvironmentID -> App (Maybe Q.Deployment)
getEnvLastDeployment eId = dataFetch (GetEnvLastDeployment eId)

getOnboarding :: App Q.Onboarding
getOnboarding = dataFetch GetOnboarding

getProject :: Q.ProjectID -> App (Maybe Q.Project)
getProject pId = dataFetch (GetProject pId)

getSlackAccessToken :: App (Maybe Q.SlackAccessToken)
getSlackAccessToken = dataFetch GetSlackAccessToken

getSlackProjectIntegration :: Q.ProjectID -> App (Maybe Q.SlackProjectIntegration)
getSlackProjectIntegration pId = dataFetch (GetSlackProjectIntegration pId)

getUser :: Q.UserID -> App (Maybe Q.User)
getUser uId = dataFetch (GetUser uId)

listBuilds :: (Int, Int) -> Maybe Q.ProjectID -> App [Q.Build]
listBuilds pagination projectId = dataFetch (ListBuilds pagination projectId)

listEnvironments :: Q.ProjectID -> App [Q.Environment]
listEnvironments = dataFetch . ListEnvironments

listProjects :: App [Q.Project]
listProjects = dataFetch ListProjects

listSlackChannels :: Text -> App [Api.SlackChannel]
listSlackChannels = dataFetch . ListSlackChannels

-- Implementation
data DatabaseQuery a where
  GetBuild :: Q.BuildID -> DatabaseQuery (Maybe Q.Build)
  GetCompany :: DatabaseQuery (Maybe Q.Company)
  GetEnvironment :: Q.EnvironmentID -> DatabaseQuery (Maybe Q.Environment)
  GetEnvLastDeployment :: Q.EnvironmentID -> DatabaseQuery (Maybe Q.Deployment)
  GetOnboarding :: DatabaseQuery Q.Onboarding
  GetProject :: Q.ProjectID -> DatabaseQuery (Maybe Q.Project)
  GetSlackAccessToken :: DatabaseQuery (Maybe Q.SlackAccessToken)
  GetSlackProjectIntegration :: Q.ProjectID -> DatabaseQuery (Maybe Q.SlackProjectIntegration)
  GetUser :: Q.UserID -> DatabaseQuery (Maybe Q.User)
  ListProjects :: DatabaseQuery [Q.Project]
  ListBuilds :: (Int, Int) -> Maybe Q.ProjectID -> DatabaseQuery [Q.Build]
  ListEnvironments :: Q.ProjectID -> DatabaseQuery [Q.Environment]
  ListSlackChannels :: Text -> DatabaseQuery [Api.SlackChannel]
  deriving (Typeable)

deriving instance Eq (DatabaseQuery a)

instance Hashable (DatabaseQuery a) where
  hashWithSalt s (GetProject a) = hashWithSalt s (0 :: Int, a)
  hashWithSalt s ListProjects = hashWithSalt s (1 :: Int)
  hashWithSalt s (ListBuilds (page, pageSize) pId) = hashWithSalt s (2 :: Int, page, pageSize, pId)
  hashWithSalt s (ListEnvironments a) = hashWithSalt s (3 :: Int, a)
  hashWithSalt s (GetEnvLastDeployment a) = hashWithSalt s (4 :: Int, a)
  hashWithSalt s (GetBuild a) = hashWithSalt s (5 :: Int, a)
  hashWithSalt s (GetSlackProjectIntegration a) = hashWithSalt s (6 :: Int, a)
  hashWithSalt s (GetEnvironment a) = hashWithSalt s (7 :: Int, a)
  hashWithSalt s (GetUser a) = hashWithSalt s (8 :: Int, a)
  hashWithSalt s GetCompany = hashWithSalt s (9 :: Int)
  hashWithSalt s GetOnboarding = hashWithSalt s (10 :: Int)
  hashWithSalt s GetSlackAccessToken = hashWithSalt s (11 :: Int)
  hashWithSalt s (ListSlackChannels a) = hashWithSalt s (12 :: Int, a)

deriving instance Show (DatabaseQuery a)

instance ShowP DatabaseQuery where
  showp = show

instance StateKey DatabaseQuery where
  data State DatabaseQuery = UserState{}

instance DataSourceName DatabaseQuery where
  dataSourceName _ = "GraphQLDatabase"

instance DataSource AppEnv DatabaseQuery where
  fetch _ _ e =
    SyncFetch $ \blockedFetches ->
      runDb (appEnv e) $ do
        getBuild_ e blockedFetches
        getCompany_ e blockedFetches
        getEnvironment_ e blockedFetches
        getOnboarding_ e blockedFetches
        getProject_ e blockedFetches
        getEnvLastDeployment_ e blockedFetches
        getSlackAccessToken_ e blockedFetches
        getSlackProjectIntegration_ e blockedFetches
        getUser_ e blockedFetches
        listProjects_ e blockedFetches
        listBuilds_ e blockedFetches
        listEnvironments_ e blockedFetches
        listSlackChannels_ e blockedFetches

getBuild_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getBuild_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.buildId (Q.listBuildsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetBuild id_) r <- blockedFetches]

getCompany_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getCompany_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- Q.getCompany currentCompanyId
    mapM_ (liftIO . flip putSuccess company) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [r | BlockedFetch GetCompany r <- blockedFetches]

getEnvironment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getEnvironment_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.envId (Q.listEnvironmentsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetEnvironment id_) r <- blockedFetches]

getEnvLastDeployment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getEnvLastDeployment_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.deploymentEnvId (Q.listEnvsLastDeployments currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(eId, r) | BlockedFetch (GetEnvLastDeployment eId) r <- blockedFetches]

getOnboarding_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getOnboarding_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- Q.getOnboarding currentCompanyId
    mapM_ (liftIO . flip putSuccess company) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [r | BlockedFetch GetOnboarding r <- blockedFetches]

getProject_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getProject_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.projectId (Q.listProjectsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (GetProject pId) r <- blockedFetches]

getSlackAccessToken_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getSlackAccessToken_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- Q.getSlackAccessToken currentCompanyId
    mapM_ (liftIO . flip putSuccess company) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [r | BlockedFetch GetSlackAccessToken r <- blockedFetches]

getSlackProjectIntegration_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getSlackProjectIntegration_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.spiProjectId (Q.listSlackProjectIntegrations currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetSlackProjectIntegration id_) r <- blockedFetches]

getUser_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
getUser_ AppEnv {..} blockedFetches =
  runFetchByID requests Q.userId (Q.listUsersById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetUser id_) r <- blockedFetches]

listEnvironments_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
listEnvironments_ AppEnv {..} blockedFetches = do
  results <- Q.listEnvironments currentCompanyId (map fst requests)
  mapM_ (\(pId, request) -> liftIO $ putSuccess request (List.filter (\e -> Q.envProjectId e == pId) results)) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (ListEnvironments pId) r <- blockedFetches]

listBuilds_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
listBuilds_ e blockedFetches = mapM_ (paginatedListBuilds_ e) requests
  where
    requests = [(pagination, projectId, r) | BlockedFetch (ListBuilds pagination projectId) r <- blockedFetches]

paginatedListBuilds_ :: AppEnv -> ((Int, Int), Maybe Q.ProjectID, ResultVar [Q.Build]) -> Db ()
paginatedListBuilds_ AppEnv {..} ((page, pageSize), projectId, request) =
  runFetchAll [request] (Q.listBuilds (pageSize, (page - 1) * pageSize) projectId currentCompanyId)
  where
    currentCompanyId = userCompanyId currentUser

listProjects_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
listProjects_ AppEnv {..} blockedFetches = runFetchAll requests (Q.listProjects currentCompanyId)
  where
    requests = [r | BlockedFetch ListProjects r <- blockedFetches]
    currentCompanyId = userCompanyId currentUser

listSlackChannels_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> Db ()
listSlackChannels_ AppEnv {..} blockedFetches = do
  results <- Api.listSlackConversations (map fst requests)
  traverse_
    (\(token, r) -> liftIO $ putSuccess r (snd (fromMaybe (token, []) (List.find (\(t, _) -> t == token) results))))
    requests
  where
    requests = [(token, r) | BlockedFetch (ListSlackChannels token) r <- blockedFetches]

runFetchByID :: Eq id => [(id, ResultVar (Maybe a))] -> (a -> id) -> Db [a] -> Db ()
runFetchByID requests readId m =
  unless (null requests) $ do
    results <- m
    mapM_
      (\(id_, request) -> liftIO $ putSuccess request (List.find (\result -> readId result == id_) results))
      requests

runFetchAll :: [ResultVar a] -> Db a -> Db ()
runFetchAll requests m =
  unless (null requests) $ do
    results <- m
    mapM_ (liftIO . flip putSuccess results) requests
