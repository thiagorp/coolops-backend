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

type UserName = Validated (SizeGreaterThan 2) Text

type UserEmail = EmailAddress

type CompanyID = Key Company

type CompanyName = Validated (SizeGreaterThan 2) Text

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

type RawPassword = Validated (SizeGreaterThan 8) ByteString

newtype SafePassword =
  SafePassword ByteString
  deriving (ToField, FromField)

protectPassword :: (MonadIO m) => RawPassword -> m SafePassword
protectPassword p = liftIO $ SafePassword <$> makePassword (getValue p) 17

userPassword_ :: User -> ByteString
userPassword_ = extract . userPassword
  where
    extract (SafePassword password) = password

authenticate :: User -> Text -> Bool
authenticate user password = verifyPassword (encodeUtf8 password) $ userPassword_ user

accessTokenTextM :: (MonadIO m) => AccessToken -> m Text
accessTokenTextM (AccessToken token) =
  case decodeUtf8' token of
    Right t -> return t
    Left e -> error (show e)

genAccessToken :: (MonadIO m) => m AccessToken
genAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m AccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ AccessToken token
    _ -> genAccessToken' (size - 1) (token <> randomChar)
