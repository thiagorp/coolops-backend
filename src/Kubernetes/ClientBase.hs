module Kubernetes.ClientBase
  ( Action(..)
  , kubernetesRequest
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.Classes

data Action
  = CreateJob ByteString
  | GetJob ByteString
  | GetPodForJob ByteString

type KubernetesMonad m = (HasHttp m, HasKubernetesSettings m)

kubernetesRequest ::
     (KubernetesMonad m) => Action -> m (Response LBS.ByteString)
kubernetesRequest action = do
  namespace <- k8sNamespace
  request <- baseRequest
  makeRequest $ buildRequest request action namespace

buildRequest :: Request -> Action -> ByteString -> Request
buildRequest request action namespace =
  case action of
    GetPodForJob jobName ->
      request
        { method = "GET"
        , path =
            "/api/v1/namespaces/" <> namespace <>
            "/pods/?labelSelector=job-name=" <>
            jobName
        }
    GetJob jobName ->
      request
        { method = "GET"
        , path =
            "/apis/batch/v1/namespaces/" <> namespace <> "/jobs/" <> jobName
        }
    CreateJob body ->
      request
        { method = "POST"
        , path = "/apis/batch/v1/namespaces/" <> namespace <> "/jobs"
        , requestBody = RequestBodyBS body
        }

baseRequest :: (HasKubernetesSettings m) => m Request
baseRequest = do
  token <- k8sToken
  host <- k8sHost
  return $
    (parseRequest_ (Text.unpack host)) {requestHeaders = defaultHeaders token}

defaultHeaders :: ByteString -> [Header]
defaultHeaders token =
  [(hContentType, "application/yaml"), (hAuthorization, "Bearer " <> token)]
