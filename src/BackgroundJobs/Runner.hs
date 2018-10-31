module BackgroundJobs.Runner
  ( JobConfig(..)
  , JobReturnType
  , RunMonad
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

import BackgroundJobs.Database

type Seconds = NominalDiffTime

data JobReturnType
  = FinishedWithSuccess
  | FinishedWithFailure Text
  | RetryImmediately Text
  | RetryIn Text
            Seconds

type RunMonad m = (HasDb m)

data JobConfig m a = JobConfig
  { deserialize :: Text -> Value -> Maybe a
  , serialize :: a -> (Text, Value)
  , run :: Int -> a -> m JobReturnType
  }

retry :: RunMonad m => Text -> m JobReturnType
retry = return . RetryImmediately

retryIn :: RunMonad m => Text -> Seconds -> m JobReturnType
retryIn reason = return . RetryIn reason

finishWithFailure :: (RunMonad m) => Text -> m JobReturnType
finishWithFailure = return . FinishedWithFailure

finishWithSuccess :: RunMonad m => m JobReturnType
finishWithSuccess = return FinishedWithSuccess

queue :: (MonadIO m) => JobConfig m job -> job -> Db m BackgroundJobId
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

runNext :: (RunMonad m) => JobConfig m job -> m ()
runNext config = do
  maybeJob <- runDb getNextJob
  forM_ maybeJob (runNext' config)

runNext' :: (RunMonad m) => JobConfig m job -> Entity BackgroundJob -> m ()
runNext' config@JobConfig {..} job@(Entity _ BackgroundJob {..}) = do
  let deserializedJob = deserialize backgroundJobName backgroundJobParams
  case deserializedJob of
    Nothing -> update' job (FinishedWithFailure "Serialization failure - Invalid payload")
    Just j -> runDeserializedJob config j job

runDeserializedJob :: (RunMonad m) => JobConfig m job -> job -> Entity BackgroundJob -> m ()
runDeserializedJob JobConfig {..} deserializedJob job@(Entity _ BackgroundJob {..}) = do
  r <- run backgroundJobRetryCount deserializedJob
  update' job r

update' :: (RunMonad m) => Entity BackgroundJob -> JobReturnType -> m ()
update' (Entity jobId BackgroundJob {..}) r = do
  now <- liftIO getCurrentTime
  let newRetryCount = retryCountFromReturn backgroundJobRetryCount r
  newNextRetry <- nextRetryFromReturn r
  newFinishedAt <- finishedFromReturn r
  let newFailureReason = failureReasonFromReturn r
  runDb $
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

finishedFromReturn :: MonadIO m => JobReturnType -> m (Maybe UTCTime)
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

nextRetryFromReturn :: MonadIO m => JobReturnType -> m (Maybe UTCTime)
nextRetryFromReturn r =
  case r of
    FinishedWithSuccess -> return Nothing
    FinishedWithFailure _ -> return Nothing
    RetryImmediately _ -> Just <$> liftIO getCurrentTime
    RetryIn _ seconds -> Just <$> (liftIO getCurrentTime >>= addSeconds seconds)

addSeconds :: MonadIO m => Seconds -> UTCTime -> m UTCTime
addSeconds seconds time = return $ addUTCTime seconds time
