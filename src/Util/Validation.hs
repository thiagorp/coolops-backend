module Util.Validation where

import RIO
import qualified RIO.Text as Text

import Util.EmailAddress

data Validation b a
  = Invalid b
  | Valid a

instance Functor (Validation b) where
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid a) = Valid (f a)

instance (Semigroup b, Monoid b) => Applicative (Validation b) where
  pure = Valid
  Invalid e1 <*> Invalid e2 = Invalid (e1 <> e2)
  Invalid e1 <*> Valid _ = Invalid e1
  Valid _ <*> Invalid e2 = Invalid e2
  Valid f <*> Valid a = Valid (f a)

data ValidationError
  = ValidationTooShort Int
  | ValidationRequired
  | ValidationInvalidEmail

type Validated a = Validation [ValidationError] a

also :: (a -> Validated a) -> (a -> Validated a) -> a -> Validated a
also validation1 validation2 value =
  case (validation1 value, validation2 value) of
    (Invalid e1, Invalid e2) -> Invalid (e1 <> e2)
    (Invalid e1, _) -> Invalid e1
    (_, Invalid e2) -> Invalid e2
    _ -> Valid value

andThen :: Validated a -> (a -> Validated b) -> Validated b
andThen validated validation =
  case validated of
    Invalid e -> Invalid e
    Valid value -> validation value

validateMinLength :: Int -> Text -> Validated Text
validateMinLength size text
  | Text.length text >= size = Valid text
  | otherwise = Invalid [ValidationTooShort size]

validateEmailAddress :: Text -> Validated EmailAddress
validateEmailAddress email =
  case emailAddress (encodeUtf8 email) of
    Nothing -> Invalid [ValidationInvalidEmail]
    Just validEmail -> Valid validEmail

validateRequired :: Maybe a -> Validated a
validateRequired value =
  case value of
    Nothing -> Invalid [ValidationRequired]
    Just a -> Valid a

valid :: a -> Validated a
valid = Valid

withDefault :: a -> Maybe a -> Validated a
withDefault def maybeValue = Valid (fromMaybe def maybeValue)
