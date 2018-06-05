module Api.Validation
  ( module Util.Validation
  , HasFieldName(..)
  , onField
  ) where

import RIO

import Util.Validation

type FieldName = Text

type DomainValidation a = Validation [ValidationError] a

type WebValidation a = Validation [(FieldName, [ValidationError])] a

class HasFieldName field where
  fieldName :: field -> Text

onField :: (HasFieldName f) => f -> DomainValidation a -> WebValidation a
onField field validated =
  case validated of
    Valid a -> Valid a
    Invalid b -> Invalid [(fieldName field, b)]
