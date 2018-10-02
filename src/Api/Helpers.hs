module Api.Helpers where

import RIO

import Network.HTTP.Types.Status (status422)
import Yesod.Core

import Api.Validation

renderValidationErrors :: WebValidationError -> Value
renderValidationErrors errors = object $ foldr buildObject [] errors
  where
    buildObject (name, e) obj = obj <> [name .= map validationToString e]

parseValidatedRequest :: (MonadHandler m) => (a -> WebValidation b) -> a -> m b
parseValidatedRequest fn req =
  case fn req of
    Valid b -> return b
    Invalid err -> sendStatusJSON status422 (renderValidationErrors err)
