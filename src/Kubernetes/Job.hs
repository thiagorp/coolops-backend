module Kubernetes.Job
  ( CreateJobMonad
  , JobDescription(..)
  , createJob
  ) where

import RIO
import qualified RIO.HashMap as HashMap

import Data.Yaml
import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.ClientBase

data JobDescription = JobDescription
  { dockerImage :: !Text
  , envVars :: !(HashMap Text Text)
  , name :: !Text
  }

buildEnv :: HashMap Text Text -> [Value]
buildEnv = map builder . HashMap.toList
  where
    builder (key, value) = object ["name" .= key, "value" .= value]

instance ToJSON JobDescription where
  toJSON JobDescription {..} =
    object
      [ "apiVersion" .= ("batch/v1" :: Text)
      , "kind" .= ("Job" :: Text)
      , "metadata" .= object ["name" .= name]
      , "spec" .=
        object
          [ "template" .=
            object
              [ "metadata" .= object ["name" .= name]
              , "spec" .=
                object
                  [ "restartPolicy" .= ("Never" :: Text)
                  , "containers" .=
                    array
                      [ object
                          [ "name" .= ("deployment-job" :: Text)
                          , "image" .= dockerImage
                          , "env" .= array (buildEnv envVars)
                          ]
                      ]
                  ]
              ]
          ]
      ]

type CreateJobMonad m = (HasHttp m)

createJob :: (CreateJobMonad m) => JobDescription -> m Bool
createJob jobDescription = do
  response <- k8sPost path body
  case statusCode (responseStatus response) of
    201 -> return True
    _ -> return False
  where
    path = "/apis/batch/v1/namespaces/default/jobs"
    body = encode jobDescription
