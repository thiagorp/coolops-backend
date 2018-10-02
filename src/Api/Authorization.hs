module Api.Authorization
  ( AuthenticatedUser(..)
  , AuthenticatedProject(..)
  , User(..)
  , Project(..)
  , userAuth
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
import Auth.Domain (User(..))
import Deployments.Database.Project (findProjectByAccessToken)
import Deployments.Domain.Project (Project(..))

newtype AuthenticatedUser =
  AuthenticatedUser User

newtype AuthenticatedProject =
  AuthenticatedProject Project

unauthorized :: Handler a
unauthorized = sendResponseStatus unauthorized401 ()

userAuth :: (AuthenticatedUser -> Handler a) -> Handler a
userAuth handler = do
  authToken <- readToken
  maybeUser <- findUserByAccessToken authToken
  case maybeUser of
    Just user -> handler (AuthenticatedUser user)
    Nothing -> unauthorized

projectAuth :: (AuthenticatedProject -> Handler a) -> Handler a
projectAuth handler = do
  authToken <- readToken
  maybeProject <- findProjectByAccessToken authToken
  case maybeProject of
    Just project -> handler (AuthenticatedProject project)
    Nothing -> unauthorized

readToken :: Handler Text
readToken = do
  req <- waiRequest
  let authHeader = fromMaybe "" $ lookup "authorization" (requestHeaders req)
  case parseAuthHeader (decodeUtf8Lenient authHeader) of
    Just authToken -> return authToken
    Nothing -> unauthorized

parseAuthHeader :: Text -> Maybe Text
parseAuthHeader = parseMaybe authHeaderParser

authHeaderParser :: Parsec Void Text Text
authHeaderParser = pack <$> (string' "Token " >> Parser.some asciiChar)
