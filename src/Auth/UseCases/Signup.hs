module Auth.UseCases.Signup
  ( module Model
  , Params(..)
  , SignupError(..)
  , SignupMonad
  , signup
  ) where

import RIO

import Auth.Database
import Model

data Params = Params
  { signupUserFirstName :: !UserName
  , signupUserLastName :: !UserName
  , signupUserEmail :: !EmailAddress
  , signupUserPassword :: !RawPassword
  , signupCompanyName :: !CompanyName
  }

data SignupError =
  UserAlreadyExists

signupToUser :: (MonadIO m) => CompanyId -> Params -> m User
signupToUser userCompanyId p = do
  userPassword <- protectPassword $ signupUserPassword p
  userAccessToken <- genAccessToken
  now <- liftIO getCurrentTime
  let userFirstName = signupUserFirstName p
  let userLastName = signupUserLastName p
  let userEmail = signupUserEmail p
  let userCreatedAt = now
  let userUpdatedAt = now
  return User {..}

signupToCompany :: (MonadIO m) => Params -> m Company
signupToCompany p = do
  companyAccessToken <- genAccessToken
  now <- liftIO getCurrentTime
  let companyName = signupCompanyName p
  let companyCreatedAt = now
  let companyUpdatedAt = now
  return Company {..}

type SignupMonad m = (MonadIO m, HasDb m)

signup :: SignupMonad m => Params -> m (Either SignupError (Entity User, Entity Company))
signup params =
  runDb $ do
    existingUser <- findUserByEmail (signupUserEmail params)
    case existingUser of
      Just _ -> return $ Left UserAlreadyExists
      Nothing -> do
        c <- signupToCompany params
        cId <- insert c
        u <- signupToUser cId params
        uId <- insert u
        return $ Right (Entity uId u, Entity cId c)
