module Kubernetes.Classes where

import RIO

import qualified Common.Config as Config
import Env

class (Monad m) =>
      HasKubernetesSettings m
  where
  k8sHost :: m Text
  k8sToken :: m ByteString
  k8sNamespace :: m ByteString

instance HasKubernetesSettings (RIO Env) where
  k8sHost = Config.k8sHost <$> asks kubernetesSettings
  k8sToken = Config.k8sToken <$> asks kubernetesSettings
  k8sNamespace = Config.k8sNamespace <$> asks kubernetesSettings
