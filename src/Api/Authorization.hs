module Api.Authorization
  ( userAuth
  , projectAuth
  ) where

import Import hiding (Handler)

import RIO.Text (pack)

import Network.HTTP.Types.Status
import Network.Wai (requestHeaders)
import Text.Megaparsec as Parser
import Text.Megaparsec.Char
import Yesod.Core

import Auth.Database
import Deployments.Database.Project (findProjectByAccessToken)

type Handler = HandlerFor Env

runAppInHandler :: App a -> Handler a
runAppInHandler m = do
  env <- getYesod
  runRIO env $ runDb m

unauthorized :: Handler a
unauthorized = sendResponseStatus unauthorized401 ()

userAuth :: (Entity User -> Handler a) -> Handler a
userAuth handler = do
  authToken <- readToken
  maybeUser <- runAppInHandler $ findUserByAccessToken authToken
  case maybeUser of
    Just user -> handler user
    Nothing -> unauthorized

projectAuth :: (Entity Project -> Handler a) -> Handler a
projectAuth handler = do
  authToken <- readToken
  maybeProject <- runAppInHandler $ findProjectByAccessToken authToken
  case maybeProject of
    Just project -> handler project
    Nothing -> unauthorized

readToken :: Handler AccessToken
readToken = do
  req <- waiRequest
  let authHeader = fromMaybe "" $ lookup "authorization" (requestHeaders req)
  case parseAuthHeader (decodeUtf8Lenient authHeader) of
    Just authToken -> return (AccessToken authToken)
    Nothing -> unauthorized

parseAuthHeader :: Text -> Maybe Text
parseAuthHeader = parseMaybe authHeaderParser

authHeaderParser :: Parsec Void Text Text
authHeaderParser = pack <$> (string' "Token " >> Parser.some asciiChar)
