module Authorization where

import RIO hiding (some)
import RIO.Text (pack)
import RIO.Text.Lazy (toStrict)

import Network.HTTP.Types.Status
import Text.Megaparsec
import Text.Megaparsec.Char
import Web.Scotty.Trans

import Auth.Classes
import Auth.Domain
import Types

data AuthorizedCompany =
  AuthorizedCompany CompanyID

unauthorized :: WebMonad a
unauthorized = do
  status status401
  finish

companyAuth :: (AuthorizedCompany -> WebMonad ()) -> WebMonad ()
companyAuth handler = do
  authToken <- readToken
  maybeCompany <- lift $ findCompanyByAccessToken authToken
  case maybeCompany of
    Just Company {companyId = key} -> handler (AuthorizedCompany key)
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
