module Auth.Domain
  ( AccessToken(..)
  , Company(..)
  , CompanyID
  , User(..)
  , UserName
  , UserEmail
  , UserID
  , CompanyName
  , RawPassword
  , SafePassword
  , accessTokenTextM
  , authenticate
  , buildCompanyName
  , buildEmailAddress
  , buildPassword
  , buildUserName
  , genAccessToken
  , genID
  , keyText
  , protectPassword
  ) where

import RIO

import Util.EmailAddress
import Util.Key
import Util.Validation

import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.ByteString.Base58 (encodeBase58I, rippleAlphabet)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField
import System.Random (randomRIO)

type UserID = Key User

newtype UserName =
  UserName Text
  deriving (ToField, FromField)

type UserEmail = EmailAddress

type CompanyID = Key Company

newtype CompanyName =
  CompanyName Text
  deriving (ToField, FromField)

newtype AccessToken =
  AccessToken ByteString
  deriving (ToField, FromField)

data Company = Company
  { companyId :: !CompanyID
  , companyName :: !CompanyName
  , companyToken :: !AccessToken
  }

data User = User
  { userId :: !UserID
  , userFirstName :: !UserName
  , userLastName :: !UserName
  , userEmail :: !UserEmail
  , userPassword :: !SafePassword
  , userAccessToken :: !AccessToken
  , userCompanyId :: !CompanyID
  }

newtype RawPassword =
  RawPassword ByteString

newtype SafePassword =
  SafePassword ByteString
  deriving (ToField, FromField)

protectPassword :: (MonadIO m) => RawPassword -> m SafePassword
protectPassword (RawPassword p) = liftIO $ SafePassword <$> makePassword p 17

userPassword_ :: User -> ByteString
userPassword_ = extract . userPassword
  where
    extract (SafePassword password) = password

authenticate :: User -> RawPassword -> Bool
authenticate user (RawPassword password) =
  verifyPassword password $ userPassword_ user

accessTokenTextM :: (MonadIO m) => AccessToken -> m Text
accessTokenTextM (AccessToken token) =
  case decodeUtf8' token of
    Right t -> return t
    Left e -> error (show e)

buildUserName :: Text -> Validated UserName
buildUserName name = UserName <$> validateMinLength 2 name

buildCompanyName :: Text -> Validated CompanyName
buildCompanyName name = CompanyName <$> validateMinLength 2 name

buildEmailAddress :: Text -> Validated EmailAddress
buildEmailAddress = validateEmailAddress

buildPassword :: Text -> Validated RawPassword
buildPassword password =
  RawPassword <$> (encodeUtf8 <$> validateMinLength 8 password)

genAccessToken :: (MonadIO m) => m AccessToken
genAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m AccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ AccessToken token
    _ -> genAccessToken' (size - 1) (token <> randomChar)
