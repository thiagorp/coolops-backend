module Kubernetes.Job
  ( CreateJobMonad
  , JobDescription(..)
  , Job(..)
  , createJob
  , deploymentContainerName
  , getJob
  , readStatus
  , JobStatus(..)
  , GetJobError
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.HashMap as HashMap

import qualified Data.Aeson as JSON
import Data.Time
import Data.Yaml
import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.Classes
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

deploymentContainerName :: Text
deploymentContainerName = "deployment-job"

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
                          [ "name" .= deploymentContainerName
                          , "image" .= dockerImage
                          , "env" .= array (buildEnv envVars)
                          ]
                      ]
                  ]
              ]
          ]
      ]

type CreateJobMonad m = (HasHttp m, HasKubernetesSettings m)

createJob :: (CreateJobMonad m) => JobDescription -> m Bool
createJob jobDescription = do
  response <- k8sPost action body
  case statusCode (responseStatus response) of
    201 -> return True
    _ -> return False
  where
    action = CreateJob
    body = encode jobDescription

data Job = Job
  { jobName :: !Text
  , jobCompletionTime :: !(Maybe UTCTime)
  , jobSucceeded :: !(Maybe Integer)
  }

instance FromJSON Job where
  parseJSON =
    withObject "job" $ \o -> do
      statusO <- o .: "status"
      metadataO <- o .: "metadata"
      jobName <- metadataO .: "name"
      jobCompletionTime <- statusO .:? "completionTime"
      jobSucceeded <- statusO .:? "succeeded"
      return Job {..}

data GetJobError
  = WrongBodyError LBS.ByteString
  | HttpStatusError Int
  deriving (Show)

instance Exception GetJobError

type GetJobMonad m = (HasHttp m, HasKubernetesSettings m, MonadThrow m)

getJob :: (GetJobMonad m) => ByteString -> m (Maybe Job)
getJob jobName = do
  response <- k8sGet (GetJob jobName)
  case statusCode (responseStatus response) of
    404 -> return Nothing
    200 ->
      case JSON.decode (responseBody response) of
        Nothing -> throwM (WrongBodyError $ responseBody response)
        Just job -> return job
    _ -> throwM (HttpStatusError $ statusCode $ responseStatus response)

data JobStatus
  = Running
  | Failed
  | Succeeded

readStatus :: Job -> JobStatus
readStatus Job {..} =
  case jobCompletionTime of
    Nothing -> Running
    Just _ ->
      if fromMaybe 0 jobSucceeded > 0
        then Succeeded
        else Failed
