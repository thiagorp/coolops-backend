module Main where

import RIO
import qualified RIO.ByteString as BS

import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS

import Auth.Database (listCompanies)
import Auth.Domain (CompanyID, companyId)
import Deployments.UseCases.RunNextDeployment as App
import Env

type AppT = RIO Env

runNextForCompany :: CompanyID -> AppT ()
runNextForCompany companyId = do
  result <- App.call companyId
  case result of
    Right _ -> BS.putStr "Ran successfully\n"
    Left App.NoDeploymentToRun -> return ()
    Left App.FailedToRunJob -> BS.putStr "Error on kubernetes\n"
    Left App.MissingEntities -> BS.putStr "Either the Project, Environment or Build for this deployment is missing\n"

app :: AppT ()
app = do
  companies <- listCompanies
  mapM_ (runNextForCompany . companyId) companies

loopWith :: Env -> IO ()
loopWith env = do
  runRIO env app
  threadDelay 1000000
  loopWith env

main :: IO ()
main = do
  requestManager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  env <- buildEnv 1 requestManager
  loopWith env
