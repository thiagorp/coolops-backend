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
  , jobFinished :: !Bool
  }

instance FromRow Job where
  fromRow = do
    jobId <- field
    jobName <- field
    jobParams <- field
    jobRetryCount <- field
    jobNextRetry <- field
    jobFinished <- field
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
      "select id, name, params, retry_count, next_retry, finished from background_jobs\
        \ where (next_retry is null or next_retry <= now()) and finished is not true\
        \ order by created_at asc\
        \ limit 1\
        \ for update skip locked"

create :: (DbMonad m) => Job -> m ()
create Job {..} = runDb' q values
  where
    values =
      (jobId, jobName, jobParams, jobRetryCount, jobNextRetry, jobFinished)
    q =
      "insert into background_jobs (id, name, params, retry_count, next_retry, finished, created_at, updated_at) values\
        \ (?, ?, ?, ?, ?, ?, now(), now())"

update :: (DbMonad m) => Job -> m ()
update Job {..} = runDb' q values
  where
    values = (jobRetryCount, jobNextRetry, jobFinished, jobId)
    q =
      "update background_jobs set (retry_count, next_retry, finished, updated_at) =\
        \ (?, ?, ?, now()) where id = ?"
