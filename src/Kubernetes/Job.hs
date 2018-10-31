module Kubernetes.Job
  ( JobDescription(..)
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
import Model hiding (DeploymentStatus(..))
import Network.HTTP.Client
import Network.HTTP.Types

import Kubernetes.ClientBase

data JobDescription = JobDescription
  { dockerImage :: !DockerImage
  , envVars :: !(HashMap Text Text)
  , name :: !DeploymentId
  }

jobRetryCount :: Int
jobRetryCount = 0

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
      , "metadata" .= object ["name" .= uuidToText deploymentId]
      , "spec" .=
        object
          [ "backoffLimit" .= jobRetryCount
          , "template" .=
            object
              [ "metadata" .= object ["name" .= name]
              , "spec" .=
                object
                  [ "restartPolicy" .= ("Never" :: Text)
                  , "containers" .=
                    array
                      [ object
                          [ "name" .= deploymentContainerName
                          , "image" .= getValue dockerImage
                          , "env" .= array (buildEnv envVars)
                          ]
                      ]
                  ]
              ]
          ]
      ]
    where
      (DeploymentKey deploymentId) = name

createJob :: (KubernetesMonad m) => JobDescription -> m Bool
createJob jobDescription = do
  response <- kubernetesRequest (CreateJob $ encode jobDescription)
  case statusCode (responseStatus response) of
    201 -> return True
    _ -> return False

data Job = Job
  { jobName :: !Text
  , jobCompletionTime :: !(Maybe UTCTime)
  , jobSucceeded :: !(Maybe Int)
  , jobFailed :: !(Maybe Int)
  }

instance FromJSON Job where
  parseJSON =
    withObject "job" $ \o -> do
      statusO <- o .: "status"
      metadataO <- o .: "metadata"
      jobName <- metadataO .: "name"
      jobCompletionTime <- statusO .:? "completionTime"
      jobSucceeded <- statusO .:? "succeeded"
      jobFailed <- statusO .:? "failed"
      return Job {..}

data GetJobError
  = WrongBodyError LBS.ByteString
  | HttpStatusError Int
  deriving (Show)

instance Exception GetJobError

type GetJobMonad m = (KubernetesMonad m, MonadThrow m)

getJob :: (GetJobMonad m) => Text -> m (Maybe Job)
getJob jobName = do
  response <- kubernetesRequest (GetJob (encodeUtf8 jobName))
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
    Nothing ->
      if fromMaybe 0 jobFailed > jobRetryCount
        then Failed
        else Running
    Just _ ->
      if fromMaybe 0 jobSucceeded > 0
        then Succeeded
        else Failed
