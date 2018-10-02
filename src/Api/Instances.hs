{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Instances
  ( Handler
  ) where

import RIO hiding (Handler)

import Data.Pool (withResource)
import Env
import Yesod.Core

import Api.Routes (Handler)
import qualified Common.Config as Config
import qualified Http.Classes as Http
import qualified Kubernetes.Classes as Kubernetes
import Slack.Api.Classes

import qualified Common.Database as DB

instance DB.HasPostgres Handler where
  getPostgresConn fx = do
    pool <- pgConnPool <$> getYesod
    withRunInIO $ \run -> withResource pool (run . fx)

instance DB.HasDBTransaction Handler where
  runTransaction tx = do
    runInnerHandler <- handlerToIO
    DB.runTransaction_ (runInnerHandler tx)
  runEitherTransaction tx = do
    runInnerHandler <- handlerToIO
    DB.runEitherTransaction_ (runInnerHandler tx)

instance Http.HasHttp Handler where
  getHttpRequestManager = requestManager <$> getYesod

instance Kubernetes.HasKubernetesSettings Handler where
  k8sHost = Config.k8sHost . kubernetesSettings <$> getYesod
  k8sToken = Config.k8sToken . kubernetesSettings <$> getYesod
  k8sNamespace = Config.k8sNamespace . kubernetesSettings <$> getYesod

instance HasSlackSettings Handler where
  slackClientId = Config.slackClientId . slackSettings <$> getYesod
  slackClientSecret = Config.slackClientSecret . slackSettings <$> getYesod
  slackVerificationToken = Config.slackVerificationToken . slackSettings <$> getYesod
