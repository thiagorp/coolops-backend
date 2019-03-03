{-# LANGUAGE RecordWildCards #-}

module Auth.UseCases.Signup
  ( Params(..)
  , SignupError(..)
  , signup
  ) where

import Import

import Auth.Database

data Params = Params
  { signupUserFirstName :: !UserName
  , signupUserLastName :: !UserName
  , signupUserEmail :: !EmailAddress
  , signupUserPassword :: !RawPassword
  , signupCompanyName :: !CompanyName
  }

data SignupError =
  UserAlreadyExists

signupToUser :: CompanyId -> Params -> App User
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

signupToCompany :: Params -> App Company
signupToCompany p = do
  companyAccessToken <- genAccessToken
  now <- liftIO getCurrentTime
  let companyName = signupCompanyName p
  let companyCreatedAt = now
  let companyUpdatedAt = now
  return Company {..}

signup :: Params -> App (Either SignupError (Entity User, Entity Company))
signup params = do
  existingUser <- findUserByEmail (signupUserEmail params)
  case existingUser of
    Just _ -> return $ Left UserAlreadyExists
    Nothing -> do
      c <- signupToCompany params
      cId <- insert c
      u <- signupToUser cId params
      uId <- insert u
      return $ Right (Entity uId u, Entity cId c)
