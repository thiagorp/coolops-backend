module Types
  ( WebError(..)
  , WebMonad
  , parseRequest
  ) where

import RIO

import Web.Scotty.Trans

import Common.App
import Validation

data WebError
  = ValidationError WebValidationError
  | StrError String

instance ScottyError WebError where
  showError = fromString . show
  stringError = StrError

instance Show WebError where
  show (ValidationError _) = "Validation error"
  show (StrError string) = string

type WebMonad = ActionT WebError AppT

parseRequest :: (a -> WebValidation b) -> a -> WebMonad b
parseRequest fn payload =
  case fn payload of
    Invalid e -> raise (ValidationError e)
    Valid b -> return b
