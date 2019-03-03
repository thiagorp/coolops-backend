{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GraphQL.Database
  ( App
  , buildEnv
  , getBuild
  , getCompany
  , getEnvironment
  , getEnvLastDeployment
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

import qualified App

import RIO
import qualified RIO.List as List

import Data.Aeson (toJSON)
import Data.Hashable
import Haxl.Core

import qualified GraphQL.Api.Calls as Api
import qualified GraphQL.Database.Queries as Q

type App = GenHaxl AppEnv

data AppEnv = AppEnv
  { currentUser :: !Q.User
  , appEnv :: !App.Env
  }

-- Util
run :: Env AppEnv -> App a -> IO a
run = runHaxl

buildEnv :: MonadIO m => Q.Entity Q.User -> App.Env -> m (Env AppEnv)
buildEnv (Q.Entity _ u) e = do
  let stateStore = stateSet UserState {} stateEmpty
  liftIO $ initEnv stateStore $ AppEnv u e

-- DB Functions
getBuild :: Q.BuildId -> App (Maybe (Q.Entity Q.Build))
getBuild bId = dataFetch (GetBuild bId)

getCompany :: App (Maybe (Q.Entity Q.Company))
getCompany = dataFetch GetCompany

getEnvLastDeployment :: Q.EnvironmentId -> App (Maybe (Q.Entity Q.Deployment))
getEnvLastDeployment eId = dataFetch (GetEnvLastDeployment eId)

getProject :: Q.ProjectId -> App (Maybe (Q.Entity Q.Project))
getProject pId = dataFetch (GetProject pId)

getEnvironment :: Q.EnvironmentId -> App (Maybe (Q.Entity Q.Environment))
getEnvironment eId = dataFetch (GetEnvironment eId)

getSlackAccessToken :: App (Maybe (Q.Entity Q.SlackAccessToken))
getSlackAccessToken = dataFetch GetSlackAccessToken

getSlackProjectIntegration :: Q.ProjectId -> App (Maybe (Q.Entity Q.SlackProjectIntegration))
getSlackProjectIntegration pId = dataFetch (GetSlackProjectIntegration pId)

getUser :: Q.UserId -> App (Maybe (Q.Entity Q.User))
getUser uId = dataFetch (GetUser uId)

listBuilds :: (Int64, Int64) -> Maybe Q.ProjectId -> App [Q.Entity Q.Build]
listBuilds pagination projectId = dataFetch (ListBuilds pagination projectId)

listEnvironments :: Q.ProjectId -> App [Q.Entity Q.Environment]
listEnvironments = dataFetch . ListEnvironments

listProjects :: App [Q.Entity Q.Project]
listProjects = dataFetch ListProjects

listSlackChannels :: Text -> App [Api.SlackChannel]
listSlackChannels = dataFetch . ListSlackChannels

-- Implementation
data DatabaseQuery a where
  GetBuild :: Q.BuildId -> DatabaseQuery (Maybe (Q.Entity Q.Build))
  GetCompany :: DatabaseQuery (Maybe (Q.Entity Q.Company))
  GetEnvLastDeployment :: Q.EnvironmentId -> DatabaseQuery (Maybe (Q.Entity Q.Deployment))
  GetProject :: Q.ProjectId -> DatabaseQuery (Maybe (Q.Entity Q.Project))
  GetEnvironment :: Q.EnvironmentId -> DatabaseQuery (Maybe (Q.Entity Q.Environment))
  GetSlackAccessToken :: DatabaseQuery (Maybe (Q.Entity Q.SlackAccessToken))
  GetSlackProjectIntegration :: Q.ProjectId -> DatabaseQuery (Maybe (Q.Entity Q.SlackProjectIntegration))
  GetUser :: Q.UserId -> DatabaseQuery (Maybe (Q.Entity Q.User))
  ListProjects :: DatabaseQuery [Q.Entity Q.Project]
  ListBuilds :: (Int64, Int64) -> Maybe Q.ProjectId -> DatabaseQuery [Q.Entity Q.Build]
  ListEnvironments :: Q.ProjectId -> DatabaseQuery [Q.Entity Q.Environment]
  ListSlackChannels :: Text -> DatabaseQuery [Api.SlackChannel]
  deriving (Typeable)

deriving instance Eq (DatabaseQuery a)

instance Hashable (DatabaseQuery a) where
  hashWithSalt s (GetProject a) = hashWithSalt s (0 :: Int, toJSON a)
  hashWithSalt s ListProjects = hashWithSalt s (1 :: Int)
  hashWithSalt s (ListBuilds (page, pageSize) pId) = hashWithSalt s (2 :: Int, page, pageSize, toJSON pId)
  hashWithSalt s (ListEnvironments a) = hashWithSalt s (3 :: Int, toJSON a)
  hashWithSalt s (GetEnvLastDeployment a) = hashWithSalt s (4 :: Int, toJSON a)
  hashWithSalt s (GetBuild a) = hashWithSalt s (5 :: Int, toJSON a)
  hashWithSalt s (GetSlackProjectIntegration a) = hashWithSalt s (6 :: Int, toJSON a)
  hashWithSalt s (GetEnvironment a) = hashWithSalt s (7 :: Int, toJSON a)
  hashWithSalt s (GetUser a) = hashWithSalt s (8 :: Int, toJSON a)
  hashWithSalt s GetCompany = hashWithSalt s (9 :: Int)
  hashWithSalt s GetSlackAccessToken = hashWithSalt s (10 :: Int)
  hashWithSalt s (ListSlackChannels a) = hashWithSalt s (11 :: Int, toJSON a)

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
      App.runApp (appEnv e) $ do
        listSlackChannels_ e blockedFetches
        getBuild_ e blockedFetches
        getCompany_ e blockedFetches
        getEnvLastDeployment_ e blockedFetches
        getProject_ e blockedFetches
        getEnvironment_ e blockedFetches
        getSlackAccessToken_ e blockedFetches
        getSlackProjectIntegration_ e blockedFetches
        getUser_ e blockedFetches
        listProjects_ e blockedFetches
        listBuilds_ e blockedFetches
        listEnvironments_ e blockedFetches

getBuild_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getBuild_ AppEnv {..} blockedFetches = runFetchById requests (Q.listBuildsById currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetBuild id_) r <- blockedFetches]

getCompany_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getCompany_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- Q.getCompany currentCompanyId
    traverse_ (liftIO . flip putSuccess company) requests
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [r | BlockedFetch GetCompany r <- blockedFetches]

getEnvironment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getEnvironment_ AppEnv {..} blockedFetches =
  runFetchById requests (Q.listEnvironmentsById currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetEnvironment id_) r <- blockedFetches]

getEnvLastDeployment_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getEnvLastDeployment_ AppEnv {..} blockedFetches =
  runFetchByFk requests Q.deploymentEnvironmentId (Q.listEnvsLastDeployments currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(eId, r) | BlockedFetch (GetEnvLastDeployment eId) r <- blockedFetches]

getProject_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getProject_ AppEnv {..} blockedFetches = runFetchById requests (Q.listProjectsById currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (GetProject pId) r <- blockedFetches]

getSlackAccessToken_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getSlackAccessToken_ AppEnv {..} blockedFetches =
  unless (null requests) $ do
    company <- Q.getSlackAccessToken currentCompanyId
    traverse_ (liftIO . flip putSuccess company) requests
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [r | BlockedFetch GetSlackAccessToken r <- blockedFetches]

getSlackProjectIntegration_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getSlackProjectIntegration_ AppEnv {..} blockedFetches =
  runFetchByFk
    requests
    Q.slackProjectIntegrationProjectId
    (Q.listSlackProjectIntegrations currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetSlackProjectIntegration id_) r <- blockedFetches]

getUser_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
getUser_ AppEnv {..} blockedFetches = runFetchById requests (Q.listUsersById currentCompanyId (map fst requests))
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(id_, r) | BlockedFetch (GetUser id_) r <- blockedFetches]

listEnvironments_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
listEnvironments_ AppEnv {..} blockedFetches = do
  results <- Q.listEnvironments currentCompanyId (map fst requests)
  traverse_
    (\(pId, request) ->
       liftIO $ putSuccess request (List.filter (\(Q.Entity _ e) -> Q.environmentProjectId e == pId) results))
    requests
  where
    currentCompanyId = Q.userCompanyId currentUser
    requests = [(pId, r) | BlockedFetch (ListEnvironments pId) r <- blockedFetches]

listBuilds_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
listBuilds_ e blockedFetches = traverse_ (paginatedListBuilds_ e) requests
  where
    requests = [(pagination, projectId, r) | BlockedFetch (ListBuilds pagination projectId) r <- blockedFetches]

paginatedListBuilds_ :: AppEnv -> ((Int64, Int64), Maybe Q.ProjectId, ResultVar [Q.Entity Q.Build]) -> App.App ()
paginatedListBuilds_ AppEnv {..} ((page, pageSize), projectId, request) =
  runFetchAll [request] (Q.listBuilds (pageSize, (page - 1) * pageSize) projectId currentCompanyId)
  where
    currentCompanyId = Q.userCompanyId currentUser

listProjects_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
listProjects_ AppEnv {..} blockedFetches = runFetchAll requests (Q.listProjects currentCompanyId)
  where
    requests = [r | BlockedFetch ListProjects r <- blockedFetches]
    currentCompanyId = Q.userCompanyId currentUser

listSlackChannels_ :: AppEnv -> [BlockedFetch DatabaseQuery] -> App.App ()
listSlackChannels_ AppEnv {..} blockedFetches = do
  results <- Api.listSlackConversations (map fst requests)
  traverse_
    (\(token, r) -> liftIO $ putSuccess r (snd (fromMaybe (token, []) (List.find (\(t, _) -> t == token) results))))
    requests
  where
    requests = [(token, r) | BlockedFetch (ListSlackChannels token) r <- blockedFetches]

runFetchByFk ::
     (Eq (Q.Key b)) => [(Q.Key b, ResultVar (Maybe (Q.Entity a)))] -> (a -> Q.Key b) -> App.App [Q.Entity a] -> App.App ()
runFetchByFk requests fk m =
  unless (null requests) $ do
    results <- m
    traverse_
      (\(id_, request) -> liftIO $ putSuccess request (List.find (\(Q.Entity _ e) -> fk e == id_) results))
      requests

runFetchById :: (Eq (Q.Key a)) => [(Q.Key a, ResultVar (Maybe (Q.Entity a)))] -> App.App [Q.Entity a] -> App.App ()
runFetchById requests m =
  unless (null requests) $ do
    results <- m
    traverse_
      (\(id_, request) -> liftIO $ putSuccess request (List.find (\(Q.Entity resultId _) -> resultId == id_) results))
      requests

runFetchAll :: [ResultVar a] -> App.App a -> App.App ()
runFetchAll requests m =
  unless (null requests) $ do
    results <- m
    traverse_ (liftIO . flip putSuccess results) requests
