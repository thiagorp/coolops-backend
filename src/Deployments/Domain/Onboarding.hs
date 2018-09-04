module Deployments.Domain.Onboarding
  ( Onboarding(..)
  , ID
  , CompanyID
  , ProjectID
  , genId
  ) where

import RIO

import Database.PostgreSQL.Simple.FromRow (FromRow)

import Auth.Domain (CompanyID)
import qualified Deployments.Domain.Project as Project
import Util.Key

type ID = Key Onboarding

type ProjectID = Project.ID

data Onboarding = Onboarding
  { onboardingId :: !ID
  , onboardingCompanyId :: !CompanyID
  , onboardingProjectId :: !(Maybe Project.ID)
  } deriving (Generic)

instance FromRow Onboarding

genId :: MonadIO m => m ID
genId = genID
