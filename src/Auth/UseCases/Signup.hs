module Auth.UseCases.Signup
  ( Params(..)
  , SignupError(..)
  , SignupMonad
  , signup
  ) where

import RIO

import Auth.Classes
import Auth.Domain
import Common.Database

data Params = Params
  { signupUserFirstName :: !UserName
  , signupUserLastName :: !UserName
  , signupUserEmail :: !UserEmail
  , signupUserPassword :: !RawPassword
  , signupCompanyName :: !CompanyName
  }

data SignupError =
  UserAlreadyExists

signupToUser :: (MonadIO m) => CompanyID -> Params -> m User
signupToUser userCompanyId p = do
  let userFirstName = signupUserFirstName p
  let userLastName = signupUserLastName p
  let userEmail = signupUserEmail p
  userPassword <- protectPassword $ signupUserPassword p
  userAccessToken <- genAccessToken
  userId <- genID
  return User {..}

signupToCompany :: (MonadIO m) => Params -> m Company
signupToCompany p = do
  let companyName = signupCompanyName p
  companyToken <- genAccessToken
  companyId <- genID
  return Company {..}

type SignupMonad m = (MonadIO m, UserRepo m, CompanyRepo m, HasDBTransaction m)

signup :: SignupMonad m => Params -> m (Either SignupError (User, Company))
signup params = do
  existingUser <- findUserByEmail (signupUserEmail params)
  case existingUser of
    Just _ -> return $ Left UserAlreadyExists
    Nothing -> do
      c <- signupToCompany params
      u <- signupToUser (companyId c) params
      runTransaction (createCompany c >> createUser u)
      return $ Right (u, c)
