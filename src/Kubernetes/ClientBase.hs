module Kubernetes.ClientBase
  ( Action(..)
  , k8sPost
  ) where

import RIO
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.Classes

data Action =
  CreateJob

k8sPath :: Action -> ByteString -> ByteString
k8sPath action namespace =
  case action of
    CreateJob -> "/apis/batch/v1/namespaces/" <> namespace <> "/jobs"

k8sBaseRequest :: (HasKubernetesSettings m) => Action -> m Request
k8sBaseRequest action = do
  token <- k8sToken
  host <- k8sHost
  namespace <- k8sNamespace
  return $
    (parseRequest_ (Text.unpack host))
      {requestHeaders = defaultHeaders token, path = k8sPath action namespace}

defaultHeaders :: ByteString -> [Header]
defaultHeaders token =
  [(hContentType, "application/yaml"), (hAuthorization, "Bearer " <> token)]

k8sPost ::
     (HasHttp m, HasKubernetesSettings m)
  => Action
  -> ByteString
  -> m (Response ByteString)
k8sPost action body = do
  baseRequest <- k8sBaseRequest action
  requestNoBody
    (baseRequest {requestBody = RequestBodyBS body, method = "POST"})
