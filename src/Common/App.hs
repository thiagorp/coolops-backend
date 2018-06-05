module Common.App where

import RIO

import qualified Database.PostgreSQL.Simple as PG

import Auth.Classes
import qualified Auth.Database as DB

newtype Env = Env
  { pgConn :: PG.Connection
  }

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader Env)

run :: AppT a -> Env -> IO a
run app = runReaderT (unAppT app)

instance DB.HasPostgresConnection AppT where
  getPostgresConn = asks pgConn

instance DB.HasPostgres AppT

instance UserRepo AppT where
  createUser = DB.createUser
  findUserByEmail = DB.findUserByEmail

instance CompanyRepo AppT where
  createCompany = DB.createCompany
  findCompanyByAccessToken = DB.findCompanyByAccessToken

instance HasDBTransaction AppT where
  runTransaction tx = ask >>= DB.runTransaction . run tx
