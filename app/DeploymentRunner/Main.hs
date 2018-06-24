module Main where

import RIO
import qualified RIO.ByteString as BS

import Database.PostgreSQL.Simple
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Auth.Classes (listCompanies)
import Auth.Domain (CompanyID, companyId)
import Common.App
import Common.Config (PGSettings(..), pgSettings)
import Deployments.UseCases.RunNextDeployment as App

runNextForCompany :: CompanyID -> AppT ()
runNextForCompany companyId = do
  result <- App.call companyId
  case result of
    Right _ -> BS.putStr "Ran successfully\n"
    Left App.NoDeploymentToRun -> BS.putStr "No deployment to run\n"
    Left App.FailedToRunJob -> BS.putStr "Error on kubernetes\n"
    Left App.MissingEntities ->
      BS.putStr
        "Either the Project, Environment or Build for this deployment is missing\n"

app :: AppT ()
app = do
  companies <- listCompanies
  _ <- mapM runNextForCompany $ map companyId companies
  return ()

loopWith :: Env -> IO ()
loopWith env = do
  run app env
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  conn <- pgSettings >>= connectPostgreSQL . pgUrl
  requestManager <-
    newTlsManagerWith
      (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv conn requestManager
  loopWith env
