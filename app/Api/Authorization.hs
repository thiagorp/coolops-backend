module Authorization
  ( AuthenticatedUser(..)
  , User(..)
  , userAuth
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
import Types (WebMonad)

data AuthenticatedUser =
  AuthenticatedUser User

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
