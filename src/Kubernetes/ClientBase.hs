module Kubernetes.ClientBase
  ( k8sPost
  ) where

import RIO

import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes

k8sHost :: String
k8sHost = "https://192.168.99.100:8443"

k8sBaseRequest :: Request
k8sBaseRequest = parseRequest_ k8sHost

apiToken :: ByteString
apiToken =
  "eyJhbGciOiJSUzI1NiIsImtpZCI6IiJ9.eyJpc3MiOiJrdWJlcm5ldGVzL3NlcnZpY2VhY2NvdW50Iiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9uYW1lc3BhY2UiOiJrdWJlLXN5c3RlbSIsImt1YmVybmV0ZXMuaW8vc2VydmljZWFjY291bnQvc2VjcmV0Lm5hbWUiOiJkZWZhdWx0LXRva2VuLXhidm1uIiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9zZXJ2aWNlLWFjY291bnQubmFtZSI6ImRlZmF1bHQiLCJrdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50L3NlcnZpY2UtYWNjb3VudC51aWQiOiI2ZTFlMmE2NS03NTY0LTExZTgtYWM0NS0wODAwMjc0NzBkMjEiLCJzdWIiOiJzeXN0ZW06c2VydmljZWFjY291bnQ6a3ViZS1zeXN0ZW06ZGVmYXVsdCJ9.OG7aJwdmdmmoHG5d2u88XtnPrb4RoYorYPjznVeLpE3VcbxL8xnxm5_oahnKijBxfkG2EWJ6zfH2WJMdYBcKYGUAJ-h7zPzLRnfAWkdaSzhvjGu2I61d0DKnQJJC5tqXDo1z_KRBfBz7osshm0OqXkGLI1Xv2MPwO1VZeZPMYar29ksWgeEtWjzoL51MsLPK8IPrGGZoN5fE-eH96zv4OfW71wTmoTgd_FIVImrlSEUYSvbe_6rpytChhdUAgxEb_69uPox8Q95jcdSqLUxz045I1-njVvg7hVZY5W_V62JGdU19hewcTDvbexorct5MP5mbpEUn-YBHsI5EosOglQ"

defaultHeaders :: [Header]
defaultHeaders =
  [(hContentType, "application/yaml"), (hAuthorization, "Bearer " <> apiToken)]

k8sPost :: (HasHttp m) => ByteString -> ByteString -> m (Response ByteString)
k8sPost path body = requestNoBody request
  where
    request =
      k8sBaseRequest
        { path = path
        , requestBody = RequestBodyBS body
        , requestHeaders = defaultHeaders
        , method = "POST"
        }
