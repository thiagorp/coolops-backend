module Api.Authorization
  ( userAuth
  , projectAuth
  ) where

import RIO hiding (Handler)
import RIO.Text (pack)

import Api.Instances

import Network.HTTP.Types.Status
import Network.Wai (requestHeaders)
import Text.Megaparsec as Parser
import Text.Megaparsec.Char
import Yesod.Core

import Auth.Database
import Deployments.Database.Project (findProjectByAccessToken)
import Model

unauthorized :: Handler a
unauthorized = sendResponseStatus unauthorized401 ()

userAuth :: (Entity User -> Handler a) -> Handler a
userAuth handler = do
  authToken <- readToken
  maybeUser <- runDb $ findUserByAccessToken authToken
  case maybeUser of
    Just user -> handler user
    Nothing -> unauthorized

projectAuth :: (Entity Project -> Handler a) -> Handler a
projectAuth handler = do
  authToken <- readToken
  maybeProject <- runDb $ findProjectByAccessToken authToken
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
