module Auth.Database
  ( findUserByAccessToken
  , findUserByEmail
  , getCompany
  , listCompanies
  ) where

import RIO hiding ((^.), on)

import Database.Esqueleto hiding (selectFirst)

import Common.PersistDatabase
import Model

findUserByEmail :: (MonadIO m) => EmailAddress -> Db m (Maybe (Entity User))
findUserByEmail email =
  selectFirst $
  from $ \u -> do
    where_ (u ^. UserEmail ==. val email)
    return u

findUserByAccessToken :: (MonadIO m) => AccessToken -> Db m (Maybe (Entity User))
findUserByAccessToken token =
  selectFirst $
  from $ \u -> do
    where_ (u ^. UserAccessToken ==. val token)
    return u

listCompanies :: (MonadIO m) => Db m [Entity Company]
listCompanies = select $ from $ \c -> return c

getCompany :: (MonadIO m) => UUID -> Db m (Maybe (Entity Company))
getCompany companyId =
  selectFirst $
  from $ \c -> do
    where_ (c ^. CompanyId ==. val (CompanyKey companyId))
    return c
