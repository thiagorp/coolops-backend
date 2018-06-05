module Api.Types
  ( WebError(..)
  , WebMonad
  ) where

import RIO

import Web.Scotty.Trans

import Common.App
import Util.Validation

data WebError
  = ValidationError [(Text, [ValidationError])]
  | StrError String

instance ScottyError WebError where
  showError = fromString . show
  stringError = StrError

instance Show WebError where
  show (ValidationError _) = "Validation error"
  show (StrError string) = string

type WebMonad = ActionT WebError AppT
