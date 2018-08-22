module GraphQL.Database
  ( App
  , buildEnv
  , getProject
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
listProjects :: App [Q.Project]
listProjects = dataFetch ListProjects

listBuilds :: (Int, Int) -> App [Q.Build]
listBuilds = dataFetch . ListBuilds

getProject :: Text -> App (Maybe Q.Project)
getProject pId = dataFetch (GetProject pId)

listEnvironments :: Text -> App [Q.Environment]
listEnvironments = dataFetch . ListEnvironments

-- Implementation
data DatabaseQuery a where
  GetProject :: Text -> DatabaseQuery (Maybe Q.Project)
  ListProjects :: DatabaseQuery [Q.Project]
  ListBuilds :: (Int, Int) -> DatabaseQuery [Q.Build]
  ListEnvironments :: Text -> DatabaseQuery [Q.Environment]
  deriving (Typeable)

deriving instance Eq (DatabaseQuery a)

instance Hashable (DatabaseQuery a) where
  hashWithSalt s (GetProject a) = hashWithSalt s (0 :: Int, a)
  hashWithSalt s ListProjects = hashWithSalt s (1 :: Int)
  hashWithSalt s (ListBuilds (page, pageSize)) =
    hashWithSalt s (2 :: Int, page, pageSize)
  hashWithSalt s (ListEnvironments a) = hashWithSalt s (3 :: Int, a)

instance Show (DatabaseQuery a) where
  show (GetProject pId) = "Fetching project " <> show pId
  show ListProjects = "Fetching all projects"
  show (ListBuilds _) = "Fetching all builds"
  show (ListEnvironments pId) = "Fetching environments for project " <> show pId

instance ShowP DatabaseQuery where
  showp = show

instance StateKey DatabaseQuery where
  data State DatabaseQuery = UserState{}

instance DataSourceName DatabaseQuery where
  dataSourceName _ = "GraphQLDatabase"

instance DataSource AppEnv DatabaseQuery where
  fetch _ _ e =
    SyncFetch $ \blockedFetches -> do
      getProject_ e blockedFetches
      listProjects_ e blockedFetches
      listBuilds_ e blockedFetches
      listEnvironments_ e blockedFetches

getProject_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
getProject_ AppEnv {..} blockedFetches =
  runFetchByID
    appEnv
    requests
    Q.projectId
    (Q.listProjectsById currentCompanyId (map fst requests))
  where
    currentCompanyId = userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (GetProject pId) r <- blockedFetches]

listEnvironments_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listEnvironments_ AppEnv {..} blockedFetches = do
  results <-
    App.run (Q.listEnvironments currentCompanyId (map fst requests)) appEnv
  mapM_
    (\(pId, request) ->
       putSuccess request (List.filter (\e -> Q.envProjectId e == pId) results))
    requests
  where
    currentCompanyId = userCompanyId currentUser
    requests =
      [(pId, r) | BlockedFetch (ListEnvironments pId) r <- blockedFetches]

listBuilds_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listBuilds_ e blockedFetches = mapM_ (paginatedListBuilds_ e) requests
  where
    requests = [(p, r) | BlockedFetch (ListBuilds p) r <- blockedFetches]

paginatedListBuilds_ :: AppEnv -> ((Int, Int), ResultVar [Q.Build]) -> IO ()
paginatedListBuilds_ AppEnv {..} ((page, pageSize), request) =
  runFetchAll
    appEnv
    [request]
    (Q.listBuilds (pageSize, (page - 1) * pageSize) currentCompanyId)
  where
    currentCompanyId = userCompanyId currentUser

listProjects_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> IO ()
listProjects_ AppEnv {..} blockedFetches =
  runFetchAll appEnv requests (Q.listProjects currentCompanyId)
  where
    requests = [r | BlockedFetch ListProjects r <- blockedFetches]
    currentCompanyId = userCompanyId currentUser

runFetchByID ::
     Eq id
  => App.Env
  -> [(id, ResultVar (Maybe a))]
  -> (a -> id)
  -> App.AppT [a]
  -> IO ()
runFetchByID e requests readId m =
  unless (null requests) $ do
    results <- App.run m e
    mapM_
      (\(id_, request) ->
         putSuccess
           request
           (List.find (\result -> readId result == id_) results))
      requests

runFetchAll :: App.Env -> [ResultVar a] -> App.AppT a -> IO ()
runFetchAll e requests m =
  unless (null requests) $ do
    results <- App.run m e
    mapM_ (`putSuccess` results) requests
