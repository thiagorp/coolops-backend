module BackgroundJobs.Runner
  ( JobConfig(..)
  , JobReturnType
  , RunMonad
  , DbMonad
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

type RunMonad m = MonadIO m

data JobConfig m a = JobConfig
  { deserialize :: Text -> Value -> Maybe a
  , serialize :: a -> (Text, Value)
  , run :: Int -> a -> m JobReturnType
  }

retry :: RunMonad m => Text -> m JobReturnType
retry = return . RetryImmediately

retryIn :: RunMonad m => Text -> Seconds -> m JobReturnType
retryIn reason = return . RetryIn reason

finishWithFailure :: RunMonad m => Text -> m JobReturnType
finishWithFailure = return . FinishedWithFailure

finishWithSuccess :: RunMonad m => m JobReturnType
finishWithSuccess = return FinishedWithSuccess

queue :: (DbMonad m) => JobConfig m job -> job -> m ()
queue JobConfig {..} job = do
  jobId <- genId
  let (jobName, jobParams) = serialize job
  let jobRetryCount = 0
  let jobNextRetry = Nothing
  let jobFinishedAt = Nothing
  let jobFailureReason = Nothing
  create Job {..}

runNext :: (RunMonad m, DbMonad m) => JobConfig m job -> m ()
runNext config = do
  maybeJob <- getNextJob
  case maybeJob of
    Nothing -> return ()
    Just job -> runNext' config job

runNext' :: DbMonad m => JobConfig m job -> Job -> m ()
runNext' config@(JobConfig {..}) job@(Job {..}) = do
  let deserializedJob = deserialize jobName jobParams
  case deserializedJob of
    Nothing ->
      update'
        job
        (FinishedWithFailure "Serialization failure - Invalid payload")
    Just j -> runDeserializedJob config j job

runDeserializedJob ::
     (RunMonad m, DbMonad m) => JobConfig m job -> job -> Job -> m ()
runDeserializedJob JobConfig {..} deserializedJob job@(Job {..}) = do
  r <- run jobRetryCount deserializedJob
  update' job r

update' :: (DbMonad m) => Job -> JobReturnType -> m ()
update' job@(Job {..}) r = do
  let newRetryCount = retryCountFromReturn jobRetryCount r
  newNextRetry <- nextRetryFromReturn r
  newFinishedAt <- finishedFromReturn r
  let newFailureReason = failureReasonFromReturn r
  update
    job
      { jobRetryCount = newRetryCount
      , jobNextRetry = newNextRetry
      , jobFinishedAt = newFinishedAt
      , jobFailureReason = newFailureReason
      }

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
    RetryIn _ seconds ->
      liftIO getCurrentTime >>= addSeconds seconds >>= return . Just

addSeconds :: MonadIO m => Seconds -> UTCTime -> m UTCTime
addSeconds seconds time = return $ addUTCTime seconds time
