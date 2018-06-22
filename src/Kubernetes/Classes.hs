module Kubernetes.Classes where

import RIO

class (Monad m) =>
      HasKubernetesSettings m
  where
  k8sHost :: m Text
  k8sToken :: m ByteString
