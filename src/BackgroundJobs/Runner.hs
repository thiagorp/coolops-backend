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
  = Done
  | RetryImmediately
  | RetryIn Seconds

type RunMonad m = MonadIO m

data JobConfig m a = JobConfig
  { deserialize :: Text -> Value -> Maybe a
  , serialize :: a -> (Text, Value)
  , run :: Int -> a -> m JobReturnType
  }

retry :: RunMonad m => m JobReturnType
retry = return RetryImmediately

retryIn :: RunMonad m => Seconds -> m JobReturnType
retryIn = return . RetryIn

finishWithFailure :: RunMonad m => m JobReturnType
finishWithFailure = return Done

finishWithSuccess :: RunMonad m => m JobReturnType
finishWithSuccess = return Done

queue :: (DbMonad m) => JobConfig m job -> job -> m ()
queue JobConfig {..} job = do
  jobId <- genId
  let (jobName, jobParams) = serialize job
  let jobRetryCount = 0
  let jobNextRetry = Nothing
  let jobFinished = False
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
    Nothing -> update' job Done
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
  let newFinished = finishedFromReturn r
  update
    job
      { jobRetryCount = newRetryCount
      , jobNextRetry = newNextRetry
      , jobFinished = newFinished
      }

finishedFromReturn :: JobReturnType -> Bool
finishedFromReturn r =
  case r of
    Done -> True
    _ -> False

retryCountFromReturn :: Int -> JobReturnType -> Int
retryCountFromReturn oldRetryCount r =
  case r of
    Done -> oldRetryCount
    RetryImmediately -> oldRetryCount + 1
    RetryIn _ -> oldRetryCount + 1

nextRetryFromReturn :: MonadIO m => JobReturnType -> m (Maybe UTCTime)
nextRetryFromReturn r =
  case r of
    Done -> return Nothing
    RetryImmediately -> Just <$> liftIO getCurrentTime
    RetryIn seconds ->
      liftIO getCurrentTime >>= addSeconds seconds >>= return . Just

addSeconds :: MonadIO m => Seconds -> UTCTime -> m UTCTime
addSeconds seconds time = return $ addUTCTime seconds time
