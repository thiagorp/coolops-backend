module Kubernetes.ClientBase
  ( k8sPost
  ) where

import RIO
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.Classes

k8sBaseRequest :: (HasKubernetesSettings m) => m Request
k8sBaseRequest = do
  token <- k8sToken
  host <- k8sHost
  return $
    (parseRequest_ (Text.unpack host)) {requestHeaders = defaultHeaders token}

defaultHeaders :: ByteString -> [Header]
defaultHeaders token =
  [(hContentType, "application/yaml"), (hAuthorization, "Bearer " <> token)]

k8sPost ::
     (HasHttp m, HasKubernetesSettings m)
  => ByteString
  -> ByteString
  -> m (Response ByteString)
k8sPost path body = do
  baseRequest <- k8sBaseRequest
  requestNoBody
    (baseRequest
       {path = path, requestBody = RequestBodyBS body, method = "POST"})
