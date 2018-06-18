module Deployments.Domain.Project
  ( Project(..)
  , ID
  , Name
  , DeploymentImage
  , AccessToken
  , CompanyID
  , buildDeploymentImage
  , buildName
  , genId
  , genAccessToken
  , deploymentImageText
  , accessTokenText
  , nameText
  ) where

import RIO

import Data.ByteString.Base58 (encodeBase58I, rippleAlphabet)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import System.Random (randomRIO)

import Auth.Domain (CompanyID)
import Util.Key
import Util.Validation

type ID = Key Project

newtype Name =
  Name Text

instance ToField Name where
  toField (Name name) = toField name

instance FromField Name where
  fromField f bs = Name <$> (fromField f bs)

newtype DeploymentImage =
  DeploymentImage Text

instance ToField DeploymentImage where
  toField (DeploymentImage image) = toField image

instance FromField DeploymentImage where
  fromField f bs = DeploymentImage <$> (fromField f bs)

newtype AccessToken =
  AccessToken ByteString

instance ToField AccessToken where
  toField (AccessToken token) = toField token

instance FromField AccessToken where
  fromField f bs = AccessToken <$> (fromField f bs)

data Project = Project
  { projectId :: !ID
  , projectName :: !Name
  , projectDeploymentImage :: !DeploymentImage
  , projectCompanyId :: !CompanyID
  , projectAccessToken :: !AccessToken
  }

genId :: MonadIO m => m ID
genId = genID

buildName :: Text -> Validated Name
buildName name = Name <$> validateMinLength 1 name

nameText :: Name -> Text
nameText (Name name) = name

accessTokenText :: AccessToken -> Text
accessTokenText (AccessToken value) =
  case decodeUtf8' value of
    Right t -> t
    Left e -> error (show e)

buildDeploymentImage :: Text -> Validated DeploymentImage
buildDeploymentImage image = DeploymentImage <$> validateMinLength 1 image

deploymentImageText :: DeploymentImage -> Text
deploymentImageText (DeploymentImage text) = text

genAccessToken :: (MonadIO m) => m AccessToken
genAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m AccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ AccessToken token
    _ -> genAccessToken' (size - 1) (token <> randomChar)
