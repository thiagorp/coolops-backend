module Authorization
  ( AuthenticatedUser(..)
  , AuthenticatedProject(..)
  , User(..)
  , Project(..)
  , userAuth
  , projectAuth
  ) where

import RIO hiding (some)
import RIO.Text (pack)
import RIO.Text.Lazy (toStrict)

import Network.HTTP.Types.Status
import Text.Megaparsec
import Text.Megaparsec.Char
import Web.Scotty.Trans

import Auth.Classes
import Auth.Domain (User(..))
import Deployments.Classes (findProjectByAccessToken)
import Deployments.Domain (Project(..))
import Types (WebMonad)

data AuthenticatedUser =
  AuthenticatedUser User

data AuthenticatedProject =
  AuthenticatedProject Project

unauthorized :: WebMonad a
unauthorized = do
  status status401
  finish

userAuth :: (AuthenticatedUser -> WebMonad ()) -> WebMonad ()
userAuth handler = do
  authToken <- readToken
  maybeUser <- lift $ findUserByAccessToken authToken
  case maybeUser of
    Just user -> handler (AuthenticatedUser user)
    Nothing -> unauthorized

projectAuth :: (AuthenticatedProject -> WebMonad ()) -> WebMonad ()
projectAuth handler = do
  authToken <- readToken
  maybeProject <- lift $ findProjectByAccessToken authToken
  case maybeProject of
    Just project -> handler (AuthenticatedProject project)
    Nothing -> unauthorized

readToken :: WebMonad Text
readToken = do
  authHeader <- fromMaybe "" <$> (header "Authorization")
  case parseAuthHeader (toStrict authHeader) of
    Just authToken -> return authToken
    Nothing -> unauthorized

parseAuthHeader :: Text -> Maybe Text
parseAuthHeader = parseMaybe authHeaderParser

authHeaderParser :: Parsec Void Text Text
authHeaderParser = string' "Token " >> some asciiChar >>= return . pack
