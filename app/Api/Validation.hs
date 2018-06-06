module Validation
  ( module Util.Validation
  , HasFieldName(..)
  , WebValidation
  , WebValidationError
  , (>>>)
  , required
  , validationToString
  ) where

import RIO

import Util.Validation

type FieldName = Text

type DomainValidation a = Validation [ValidationError] a

type WebValidationError = [(FieldName, [ValidationError])]

type WebValidation a = Validation WebValidationError a

class HasFieldName field where
  fieldName :: field -> Text

required :: (HasFieldName f) => f -> Maybe a -> (f, WebValidation a)
required field value = (field, onField field (validateRequired value))

(>>>) ::
     (HasFieldName f)
  => (f, WebValidation a)
  -> (a -> DomainValidation b)
  -> WebValidation b
(>>>) (field, preValidated) builder =
  case preValidated of
    Invalid e -> Invalid e
    Valid a -> onField field (builder a)

onField :: (HasFieldName f) => f -> DomainValidation a -> WebValidation a
onField field validated =
  case validated of
    Valid a -> Valid a
    Invalid b -> Invalid [(fieldName field, b)]

validationToString :: ValidationError -> String
validationToString v =
  case v of
    ValidationTooShort m -> "must cointain at least " <> show m <> " characters"
    ValidationRequired -> "is required"
    ValidationInvalidEmail -> "is not a valid email"
