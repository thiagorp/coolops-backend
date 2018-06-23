module Kubernetes.Pod
  ( ContainerState(..)
  , getPodForJob
  , getPodContainerState
  ) where

import RIO
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.HashMap as HashMap
import qualified RIO.List as List
import qualified RIO.Text as Text

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Network.HTTP.Types

import Http.Classes
import Kubernetes.Classes
import Kubernetes.ClientBase

data ContainerState
  = Running
  | Terminated
  | Waiting Text

data ContainerStatus = ContainerStatus
  { containerName :: !Text
  , containerState :: !ContainerState
  }

data Pod = Pod
  { podContainerStatuses :: ![ContainerStatus]
  }

newtype PodsList =
  PodsList [Pod]

data GetPodError
  = WrongBodyError Text
                   LBS.ByteString
  | HttpStatusError Int
  deriving (Show)

instance Exception GetPodError

type GetPodMonad m = (HasHttp m, HasKubernetesSettings m, MonadThrow m)

getPodForJob :: (GetPodMonad m) => ByteString -> m (Maybe Pod)
getPodForJob jobName = do
  response <- kubernetesRequest (GetPodForJob jobName)
  case statusCode (responseStatus response) of
    404 -> return Nothing
    200 ->
      case eitherDecode (responseBody response) of
        Left e -> throwM (WrongBodyError (Text.pack e) (responseBody response))
        Right (PodsList pods) ->
          case pods of
            [] -> return Nothing
            pod:_ -> return $ Just pod
    _ -> throwM (HttpStatusError $ statusCode $ responseStatus response)

findContainerStatusByName :: Text -> [ContainerStatus] -> Maybe ContainerStatus
findContainerStatusByName name =
  List.find (\status -> containerName status == name)

getPodContainerState :: Text -> Pod -> Maybe ContainerState
getPodContainerState name Pod {..} =
  case findContainerStatusByName name podContainerStatuses of
    Nothing -> Nothing
    Just ContainerStatus {..} -> Just containerState

instance FromJSON PodsList where
  parseJSON =
    withObject "pods" $ \o -> do
      podsList <- o .: "items"
      return $ PodsList podsList

instance FromJSON Pod where
  parseJSON =
    withObject "pod" $ \o -> do
      statusO <- o .: "status"
      containerStatuses <- statusO .: "containerStatuses"
      podContainerStatuses <- parseJSON containerStatuses
      return Pod {..}

instance FromJSON ContainerStatus where
  parseJSON =
    withObject "containerStatus" $ \o -> do
      containerName <- o .: "name"
      stateO <- o .:? "state" .!= HashMap.empty
      containerState <- parseContainerState stateO
      return ContainerStatus {..}

parseContainerState ::
     HashMap Text (HashMap Text Value) -> Parser ContainerState
parseContainerState =
  HashMap.foldlWithKey' convertState (return $ Waiting "Unknown")
  where
    convertState _ key stateHash =
      case key of
        "running" -> return Running
        "terminated" -> return Terminated
        "waiting" -> parseWaitingState stateHash
        _ -> return (Waiting "InvalidContainerState")

parseWaitingState :: HashMap Text Value -> Parser ContainerState
parseWaitingState stateHash = do
  let maybeReason = HashMap.lookup "reason" stateHash
  reason <- maybe (return "NoReasonOnStateHash") parseJSON maybeReason
  return $ Waiting reason
