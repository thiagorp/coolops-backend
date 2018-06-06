module Main where

import RIO

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Web.Scotty.Trans

import Common.App
import Common.Config
import Common.Database (migrateDb)
import qualified Handlers
import Types
import Util.Validation (ValidationError(..))
import Validation

newtype ValidationResponse =
  ValidationResponse WebValidationError

instance ToJSON ValidationResponse where
  toJSON (ValidationResponse errors) = object $ foldr buildObject [] errors
    where
      buildObject (name, e) obj = obj <> [name .= map validationToString e]

validationToString :: ValidationError -> String
validationToString v =
  case v of
    ValidationTooShort m -> "must cointain at least " <> show m <> " characters"
    ValidationRequired -> "is required"
    ValidationInvalidEmail -> "is not a valid email"

errorHandler :: Monad m => WebError -> ActionT WebError m ()
errorHandler e =
  case e of
    ValidationError validationErrors -> do
      status status422
      json $ ValidationResponse validationErrors
    _ -> do
      status status500
      json $ show e

corsMiddleware :: Middleware
corsMiddleware = cors $ const (Just policy)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

routes :: ScottyT WebError AppT ()
routes = do
  middleware corsMiddleware
  defaultHandler errorHandler
  post "/signup" Handlers.signup
  post "/tokens" Handlers.login

acquirePool :: IO (Pool Connection)
acquirePool = do
  settings <- pgSettings
  createPool (connection settings) close 1 10 (poolSize settings)
  where
    connection settings = connectPostgreSQL $ pgUrl settings

main :: IO ()
main = do
  pool <- acquirePool
  withResource pool migrateDb
  let runner app = withResource pool $ \conn -> run app (Env conn)
  port <- appPort
  scottyT port runner routes
