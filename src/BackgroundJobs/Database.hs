module BackgroundJobs.Database where

import RIO

import Data.Aeson
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Common.Database
import Util.Key

type DbMonad m = HasPostgres m

type JobID = Key Job

data Job = Job
  { jobId :: !JobID
  , jobName :: !Text
  , jobParams :: !Value
  , jobRetryCount :: !Int
  , jobNextRetry :: !(Maybe UTCTime)
  , jobFinishedAt :: !(Maybe UTCTime)
  , jobFailureReason :: !(Maybe Text)
  }

instance FromRow Job where
  fromRow = do
    jobId <- field
    jobName <- field
    jobParams <- field
    jobRetryCount <- field
    mJobNextRetry <- field
    let jobNextRetry = localTimeToUTC utc <$> mJobNextRetry
    mJobFinishedAt <- field
    let jobFinishedAt = localTimeToUTC utc <$> mJobFinishedAt
    jobFailureReason <- field
    return Job {..}

genId :: MonadIO m => m JobID
genId = genID

getNextJob :: (DbMonad m) => m (Maybe Job)
getNextJob = do
  result <- runQuery_ q
  case result of
    [] -> return Nothing
    row:_ -> return (Just row)
  where
    q =
      "select id, name, params, retry_count, next_retry, finished_at, failure_reason from background_jobs\
        \ where (next_retry is null or next_retry <= now() at time zone 'utc') and finished_at is null\
        \ order by created_at asc\
        \ limit 1\
        \ for update skip locked"

create :: (DbMonad m) => Job -> m ()
create Job {..} = runDb' q values
  where
    values = (jobId, jobName, jobParams, jobRetryCount, jobNextRetry, jobFinishedAt, jobFailureReason)
    q =
      "insert into background_jobs (id, name, params, retry_count, next_retry, finished_at, failure_reason, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, ?, ?, now() at time zone 'utc', now() at time zone 'utc')"

update :: (DbMonad m) => Job -> m ()
update Job {..} = runDb' q values
  where
    values = (jobRetryCount, jobNextRetry, jobFinishedAt, jobFailureReason, jobId)
    q =
      "update background_jobs set (retry_count, next_retry, finished_at, failure_reason, updated_at) =\
        \ (?, ?, ?, ?, now() at time zone 'utc') where id = ?"
