{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Util.Validation
  ( IsSlug
  , SizeGreaterThan
  , Validated
  , type (&&)
  , getValue
  ) where

import RIO
import qualified RIO.ByteString as ByteString
import qualified RIO.Text as Text

import Data.Aeson
import Data.Aeson.Types (Parser)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField
import GHC.TypeLits (KnownNat, Nat, natVal)

import Util.Slug

newtype Validated p x =
  Validated x
  deriving (FromField, ToField, ToJSON, Show)

parseJSON_ :: (Predicate p a) => a -> Parser (Validated p a)
parseJSON_ t =
  case validate t of
    Left err -> fail err
    Right x -> return x

instance (Predicate p Text) => FromJSON (Validated p Text) where
  parseJSON = withText "Text" parseJSON_

instance (Predicate p ByteString) => FromJSON (Validated p ByteString) where
  parseJSON = withText "Text" $ \t -> parseJSON_ (encodeUtf8 t)

class Predicate p x where
  validate :: x -> Either String (Validated p x)

data SizeGreaterThan (n :: Nat)

data IsSlug

data (&&) l r

validateSizeGreaterThan :: (KnownNat n) => proxy n -> (a -> Int) -> a -> Either String (Validated (SizeGreaterThan n) a)
validateSizeGreaterThan nat sizeFn text =
  if textSize > minSize
    then Right (Validated text)
    else Left errorMessage
  where
    textSize = sizeFn text
    minSize = fromIntegral $ natVal nat
    errorMessage = "needs to be bigger than " <> show minSize <> " characters"

instance (KnownNat n) => Predicate (SizeGreaterThan n) Text where
  validate = validateSizeGreaterThan (Proxy @n) Text.length

instance (KnownNat n) => Predicate (SizeGreaterThan n) ByteString where
  validate = validateSizeGreaterThan (Proxy @n) ByteString.length

instance Predicate IsSlug Text where
  validate text =
    if isValidSlug text
      then Right (Validated text)
      else Left "is not a valid slug"

instance (Predicate m x, Predicate n x) => Predicate (m && n) x where
  validate text = validate @m text >> validate @n text >> Right (Validated text)

getValue :: Validated p x -> x
getValue (Validated value) = value
