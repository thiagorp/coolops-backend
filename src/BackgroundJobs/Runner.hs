{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BackgroundJobs.Runner
  ( JobConfig(..)
  , JobReturnType
  , retry
  , retryIn
  , finishWithSuccess
  , finishWithFailure
  , queue
  , runNext
  ) where

import RIO

import Data.Aeson
import Data.Time
import Database.Persist

import BackgroundJobs.Database

type Seconds = NominalDiffTime

data JobReturnType
  = FinishedWithSuccess
  | FinishedWithFailure Text
  | RetryImmediately Text
  | RetryIn Text
            Seconds

data JobConfig a = JobConfig
  { deserialize :: Text -> Value -> Maybe a
  , serialize :: a -> (Text, Value)
  , run :: Int -> a -> App JobReturnType
  }

retry :: Text -> App JobReturnType
retry = return . RetryImmediately

retryIn :: Text -> Seconds -> App JobReturnType
retryIn reason = return . RetryIn reason

finishWithFailure :: Text -> App JobReturnType
finishWithFailure = return . FinishedWithFailure

finishWithSuccess :: App JobReturnType
finishWithSuccess = return FinishedWithSuccess

queue :: JobConfig job -> job -> App BackgroundJobId
queue JobConfig {..} job = do
  now <- liftIO getCurrentTime
  let (backgroundJobName, backgroundJobParams) = serialize job
  let backgroundJobRetryCount = 0
  let backgroundJobNextRetry = Nothing
  let backgroundJobFinishedAt = Nothing
  let backgroundJobFailureReason = Nothing
  let backgroundJobCreatedAt = now
  let backgroundJobUpdatedAt = now
  insert BackgroundJob {..}

runNext :: JobConfig job -> App ()
runNext config = do
  maybeJob <- getNextJob
  forM_ maybeJob (runNext' config)

runNext' :: JobConfig job -> Entity BackgroundJob -> App ()
runNext' config@JobConfig {..} job@(Entity _ BackgroundJob {..}) = do
  let deserializedJob = deserialize backgroundJobName backgroundJobParams
  case deserializedJob of
    Nothing -> update' job (FinishedWithFailure "Serialization failure - Invalid payload")
    Just j -> runDeserializedJob config j job

runDeserializedJob :: JobConfig job -> job -> Entity BackgroundJob -> App ()
runDeserializedJob JobConfig {..} deserializedJob job@(Entity _ BackgroundJob {..}) = do
  r <- run backgroundJobRetryCount deserializedJob
  update' job r

update' :: Entity BackgroundJob -> JobReturnType -> App ()
update' (Entity jobId BackgroundJob {..}) r = do
  now <- liftIO getCurrentTime
  let newRetryCount = retryCountFromReturn backgroundJobRetryCount r
  newNextRetry <- nextRetryFromReturn r
  newFinishedAt <- finishedFromReturn r
  let newFailureReason = failureReasonFromReturn r
  update
    jobId
    [ BackgroundJobRetryCount =. newRetryCount
    , BackgroundJobNextRetry =. newNextRetry
    , BackgroundJobFinishedAt =. newFinishedAt
    , BackgroundJobFailureReason =. newFailureReason
    , BackgroundJobUpdatedAt =. now
    ]

failureReasonFromReturn :: JobReturnType -> Maybe Text
failureReasonFromReturn job =
  case job of
    FinishedWithSuccess -> Nothing
    FinishedWithFailure t -> Just t
    RetryImmediately t -> Just t
    RetryIn t _ -> Just t

finishedFromReturn :: JobReturnType -> App (Maybe UTCTime)
finishedFromReturn r =
  case r of
    FinishedWithSuccess -> Just <$> liftIO getCurrentTime
    FinishedWithFailure _ -> Just <$> liftIO getCurrentTime
    _ -> return Nothing

retryCountFromReturn :: Int -> JobReturnType -> Int
retryCountFromReturn oldRetryCount r =
  case r of
    FinishedWithSuccess -> oldRetryCount
    FinishedWithFailure _ -> oldRetryCount
    RetryImmediately _ -> oldRetryCount + 1
    RetryIn _ _ -> oldRetryCount + 1

nextRetryFromReturn :: JobReturnType -> App (Maybe UTCTime)
nextRetryFromReturn r =
  case r of
    FinishedWithSuccess -> return Nothing
    FinishedWithFailure _ -> return Nothing
    RetryImmediately _ -> Just <$> liftIO getCurrentTime
    RetryIn _ seconds -> Just <$> (liftIO getCurrentTime >>= addSeconds seconds)

addSeconds :: Seconds -> UTCTime -> App UTCTime
addSeconds seconds time = return $ addUTCTime seconds time
