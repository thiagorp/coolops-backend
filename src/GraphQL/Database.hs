module GraphQL.Database
  ( App
  , buildEnv
  , getBuild
  , getCompany
  , getEnvironment
  , getEnvLastDeployment
  , getOnboarding
  , getProject
  , getSlackProjectIntegration
  , getUser
  , listBuilds
  , listEnvironments
  , listProjects
  , run
  ) where

import RIO
import qualified RIO.List as List

import Data.Hashable
import Haxl.Core

import Auth.Domain (User(..))
import qualified Common.App as App
import qualified GraphQL.Database.Queries as Q

type App = GenHaxl AppEnv

data AppEnv = AppEnv
  { currentUser :: !User
  , appEnv :: !App.Env
  }

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

getSlackProjectIntegration :: Q.ProjectID -> App (Maybe Q.SlackProjectIntegration)
getSlackProjectIntegration pId = dataFetch (GetSlackProjectIntegration pId)

getUser :: Q.UserID -> App (Maybe Q.User)
getUser uId = dataFetch (GetUser uId)

listBuilds :: (Int, Int) -> App [Q.Build]
listBuilds = dataFetch . ListBuilds

listEnvironments :: Q.ProjectID -> App [Q.Environment]
listEnvironments = dataFetch . ListEnvironments

listProjects :: App [Q.Project]
listProjects = dataFetch ListProjects

-- Implementation
data DatabaseQuery a where
  GetBuild :: Q.BuildID -> DatabaseQuery (Maybe Q.Build)
  GetCompany :: DatabaseQuery (Maybe Q.Company)
  GetEnvironment :: Q.EnvironmentID -> DatabaseQuery (Maybe Q.Environment)
  GetEnvLastDeployment :: Q.EnvironmentID -> DatabaseQuery (Maybe Q.Deployment)
  GetOnboarding :: DatabaseQuery Q.Onboarding
  GetProject :: Q.ProjectID -> DatabaseQuery (Maybe Q.Project)
  GetSlackProjectIntegration :: Q.ProjectID -> DatabaseQuery (Maybe Q.SlackProjectIntegration)
  GetUser :: Q.UserID -> DatabaseQuery (Maybe Q.User)
  ListProjects :: DatabaseQuery [Q.Project]
  ListBuilds :: (Int, Int) -> DatabaseQuery [Q.Build]
  ListEnvironments :: Q.ProjectID -> DatabaseQuery [Q.Environment]
  deriving (Typeable)

deriving instance Eq (DatabaseQuery a)

instance Hashable (DatabaseQuery a) where
  hashWithSalt s (GetProject a) = hashWithSalt s (0 :: Int, a)
  hashWithSalt s ListProjects = hashWithSalt s (1 :: Int)
  hashWithSalt s (ListBuilds (page, pageSize)) = hashWithSalt s (2 :: Int, page, pageSize)
  hashWithSalt s (ListEnvironments a) = hashWithSalt s (3 :: Int, a)
  hashWithSalt s (GetEnvLastDeployment a) = hashWithSalt s (4 :: Int, a)
  hashWithSalt s (GetBuild a) = hashWithSalt s (5 :: Int, a)
  hashWithSalt s (GetSlackProjectIntegration a) = hashWithSalt s (6 :: Int, a)
  hashWithSalt s (GetEnvironment a) = hashWithSalt s (7 :: Int, a)
  hashWithSalt s (GetUser a) = hashWithSalt s (8 :: Int, a)
  hashWithSalt s GetCompany = hashWithSalt s (9 :: Int)
  hashWithSalt s GetOnboarding = hashWithSalt s (10 :: Int)

deriving instance Show (DatabaseQuery a)

instance ShowP DatabaseQuery where
  showp = show

instance StateKey DatabaseQuery where
  data State DatabaseQuery = UserState{}

instance DataSourceName DatabaseQuery where
  dataSourceName _ = "GraphQLDatabase"

instance DataSource AppEnv DatabaseQuery where
  fetch _ _ e =
    SyncFetch $ \blockedFetches -> do
      getBuild_ e blockedFetches
      getCompany_ e blockedFetches
      getEnvironment_ e blockedFetches
      getOnboarding_ e blockedFetches
      getProject_ e blockedFetches
      getEnvLastDeployment_ e blockedFetches
      getSlackProjectIntegration_ e blockedFetches
      getUser_ e blockedFetches
      listProjects_ e blockedFetches
      listBuilds_ e blockedFetches
      listEnvironments_ e blockedFetches

getBuild_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getBuild_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.buildId (Q.listBuildsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetBuild id_) r <- blockedFetches]

getCompany_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getCompany_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- App.run (Q.getCompany currentCompanyId) appEnv
    mapM_ (`putSuccess` company) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [r | BlockedFetch GetCompany r <- blockedFetches]

getEnvironment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getEnvironment_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.envId (Q.listEnvironmentsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetEnvironment id_) r <- blockedFetches]

getEnvLastDeployment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getEnvLastDeployment_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.deploymentEnvId (Q.listEnvsLastDeployments currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(eId, r) | BlockedFetch (GetEnvLastDeployment eId) r <- blockedFetches]

getOnboarding_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getOnboarding_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- App.run (Q.getOnboarding currentCompanyId) appEnv
    mapM_ (`putSuccess` company) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [r | BlockedFetch GetOnboarding r <- blockedFetches]

getProject_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getProject_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.projectId (Q.listProjectsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (GetProject pId) r <- blockedFetches]

getSlackProjectIntegration_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getSlackProjectIntegration_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.spiProjectId (Q.listSlackProjectIntegrations currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetSlackProjectIntegration id_) r <- blockedFetches]

getUser_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getUser_ AppEnv {..} blockedFetches =
  runFetchByID appEnv requests Q.userId (Q.listUsersById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetUser id_) r <- blockedFetches]

listEnvironments_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listEnvironments_ AppEnv {..} blockedFetches = do
  results <- App.run (Q.listEnvironments currentCompanyId (map fst requests)) appEnv
  mapM_ (\(pId, request) -> putSuccess request (List.filter (\e -> Q.envProjectId e == pId) results)) requests
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (ListEnvironments pId) r <- blockedFetches]

listBuilds_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listBuilds_ e blockedFetches = mapM_ (paginatedListBuilds_ e) requests
  where
    requests = [(p, r) | BlockedFetch (ListBuilds p) r <- blockedFetches]

paginatedListBuilds_ :: AppEnv -> ((Int, Int), ResultVar [Q.Build]) -> IO ()
paginatedListBuilds_ AppEnv {..} ((page, pageSize), request) =
  runFetchAll appEnv [request] (Q.listBuilds (pageSize, (page - 1) * pageSize) currentCompanyId)
  where
    currentCompanyId = userCompanyId currentUser

listProjects_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listProjects_ AppEnv {..} blockedFetches = runFetchAll appEnv requests (Q.listProjects currentCompanyId)
  where
    requests = [r | BlockedFetch ListProjects r <- blockedFetches]
    currentCompanyId = userCompanyId currentUser

runFetchByID :: Eq id => App.Env -> [(id, ResultVar (Maybe a))] -> (a -> id) -> App.AppT [a] -> IO ()
runFetchByID e requests readId m =
  unless (null requests) $ do
    results <- App.run m e
    mapM_ (\(id_, request) -> putSuccess request (List.find (\result -> readId result == id_) results)) requests

runFetchAll :: App.Env -> [ResultVar a] -> App.AppT a -> IO ()
runFetchAll e requests m =
  unless (null requests) $ do
    results <- App.run m e
    mapM_ (`putSuccess` results) requests
