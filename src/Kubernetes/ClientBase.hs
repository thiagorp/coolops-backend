module Kubernetes.ClientBase
  ( Action(..)
  , kubernetesRequest
  ) where

import Import
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as Text

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes

data Action
  = CreateJob ByteString
  | GetJob ByteString
  | GetPodForJob ByteString
  | GetPodLogs (Maybe Int)
               ByteString

kubernetesRequest :: Action -> App (Response LBS.ByteString)
kubernetesRequest action = do
  settings <- kubernetesSettings <$> getEnv
  let namespace = k8sNamespace settings
  request <- baseRequest
  makeRequest $ buildRequest request action namespace

buildRequest :: Request -> Action -> ByteString -> Request
buildRequest request action namespace =
  case action of
    GetPodForJob jobName ->
      request
        {method = "GET", path = "/api/v1/namespaces/" <> namespace <> "/pods/?labelSelector=job-name%3D" <> jobName}
    GetJob jobName -> request {method = "GET", path = "/apis/batch/v1/namespaces/" <> namespace <> "/jobs/" <> jobName}
    CreateJob body ->
      request
        {method = "POST", path = "/apis/batch/v1/namespaces/" <> namespace <> "/jobs", requestBody = RequestBodyBS body}
    GetPodLogs tailSize podName ->
      request
        { method = "GET"
        , path =
            "/api/v1/namespaces/" <> namespace <> "/pods/" <> podName <> "/log" <>
            maybe "" (\n -> "?tailLines=" <> encodeUtf8 (tshow n)) tailSize
        }

baseRequest :: App Request
baseRequest = do
  settings <- kubernetesSettings <$> getEnv
  let token = k8sToken settings
  let host = k8sHost settings
  return $ (parseRequest_ (Text.unpack host)) {requestHeaders = defaultHeaders token}

defaultHeaders :: ByteString -> [Header]
defaultHeaders token = [(hContentType, "application/yaml"), (hAuthorization, "Bearer " <> token)]
