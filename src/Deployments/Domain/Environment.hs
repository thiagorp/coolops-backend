{-# LANGUAGE TypeOperators #-}

module Deployments.Domain.Environment
  ( ID
  , ProjectID
  , Name
  , Environment(..)
  , Slug
  , genId
  , environmentId_
  , environmentName_
  , nameText
  ) where

import RIO

import qualified Deployments.Domain.Project as Project
import Util.Key
import Util.Validation

type ID = Key Environment

type ProjectID = Project.ID

type Name = Validated (SizeGreaterThan 1) Text

type Slug = Validated (IsSlug && SizeGreaterThan 1) Text

data Environment = Environment
  { environmentId :: !ID
  , environmentName :: !Name
  , environmentEnvVars :: !(HashMap Text Text)
  , environmentProjectId :: !ProjectID
  , environmentSlug :: !Slug
  } deriving (Show)

genId :: MonadIO m => m ID
genId = genID

environmentId_ :: Environment -> Text
environmentId_ Environment {..} = keyText environmentId

environmentName_ :: Environment -> Text
environmentName_ Environment {..} = nameText environmentName

nameText :: Name -> Text
nameText = getValue
