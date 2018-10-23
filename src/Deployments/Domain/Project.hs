{-# LANGUAGE TypeOperators #-}

module Deployments.Domain.Project
  ( Project(..)
  , ID
  , Name
  , DeploymentImage
  , AccessToken
  , CompanyID
  , Slug
  , genId
  , genAccessToken
  , deploymentImageText
  , accessTokenText
  , nameText
  , projectAccessToken_
  , projectDeploymentImage_
  , projectId_
  , projectName_
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

type Name = Validated (SizeGreaterThan 1) Text

type DeploymentImage = Validated (SizeGreaterThan 1) Text

type Slug = Validated (IsSlug && SizeGreaterThan 1) Text

newtype AccessToken =
  AccessToken ByteString
  deriving (Show, ToField, FromField)

data Project = Project
  { projectId :: !ID
  , projectName :: !Name
  , projectDeploymentImage :: !DeploymentImage
  , projectCompanyId :: !CompanyID
  , projectAccessToken :: !AccessToken
  , projectSlug :: !Slug
  } deriving (Show)

genId :: MonadIO m => m ID
genId = genID

projectId_ :: Project -> Text
projectId_ Project {..} = keyText projectId

nameText :: Name -> Text
nameText = getValue

projectName_ :: Project -> Text
projectName_ Project {..} = nameText projectName

accessTokenText :: AccessToken -> Text
accessTokenText (AccessToken value) =
  case decodeUtf8' value of
    Right t -> t
    Left e -> error (show e)

projectAccessToken_ :: Project -> Text
projectAccessToken_ Project {..} = accessTokenText projectAccessToken

deploymentImageText :: DeploymentImage -> Text
deploymentImageText = getValue

projectDeploymentImage_ :: Project -> Text
projectDeploymentImage_ Project {..} = deploymentImageText projectDeploymentImage

genAccessToken :: (MonadIO m) => m AccessToken
genAccessToken = genAccessToken' 36 ""

genAccessToken' :: (MonadIO m) => Integer -> ByteString -> m AccessToken
genAccessToken' size token = do
  i <- liftIO $ randomRIO (0, 57)
  let randomChar = encodeBase58I rippleAlphabet i
  case size of
    0 -> return $ AccessToken token
    _ -> genAccessToken' (size - 1) (token <> randomChar)
