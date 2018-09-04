module Deployments.Database.Onboarding
  ( HasPostgres
  , getOnboarding
  , upsertOnboarding
  ) where

import RIO

import Database.PostgreSQL.Simple

import Common.Database
import Deployments.Domain.Onboarding

getOnboarding :: (HasPostgres m) => CompanyID -> m Onboarding
getOnboarding companyId = do
  result <- runQuery q (Only companyId)
  case result of
    [] -> buildNew companyId
    row:_ -> return row
  where
    q =
      "select id, company_id, project_id from onboardings \
      \where company_id = ?"

upsertOnboarding :: (HasPostgres m) => Onboarding -> m ()
upsertOnboarding Onboarding {..} = runDb' q values
  where
    q =
      "insert into onboardings (id, company_id, project_id, created_at, updated_at)\
      \ values (?, ?, ?, now(), now())\
      \ on conflict (id) do update\
      \ set project_id = excluded.project_id, updated_at = now()"
    values = (onboardingId, onboardingCompanyId, onboardingProjectId)

buildNew :: (MonadIO m) => CompanyID -> m Onboarding
buildNew companyId = do
  onboardingId <- genId
  let onboardingCompanyId = companyId
  let onboardingProjectId = Nothing
  return Onboarding {..}
