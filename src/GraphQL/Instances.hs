{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}

module GraphQL.Instances
  ( UUID
  ) where

import RIO

import qualified Data.Aeson as J
import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist
import GraphQL.API
import GraphQL.Resolver
import GraphQL.Value
import GraphQL.Value.FromValue
import Types
import Util.EmailAddress
import Util.UUID

instance GraphQLEnum DeploymentStatus where
  enumValues = [makeName "QUEUED", makeName "RUNNING", makeName "SUCCEEDED", makeName "FAILED"]
  enumFromValue name =
    case unName name of
      "QUEUED" -> Right Queued
      "RUNNING" -> Right Running
      "SUCCEEDED" -> Right Succeeded
      "FAILED" -> Right $ Failed "fromGraphQL"
      _ -> Left "Invalid DeploymentStatus"
  enumToValue status =
    case status of
      Queued -> Name "QUEUED"
      Running -> Name "RUNNING"
      Succeeded -> Name "SUCCEEDED"
      Failed _ -> Name "FAILED"

keyToValue :: (J.ToJSON (Key a)) => Key a -> Result Value
keyToValue key =
  case J.toJSON key of
    J.String k -> Result [] (ValueString (String k))
    _ -> Result [InvalidValue (Name "Id") "Id should be UUID"] ValueNull

instance forall m a. (Applicative m, J.ToJSON (Key a)) => HasResolver m (Key a) where
  type Handler m (Key a) = m (Key a)
  resolve handler Nothing = keyToValue <$> handler
  resolve _ (Just ss) = pure (Result [SubSelectionOnLeaf ss] ValueNull)

instance HasAnnotatedType (Key a) where
  getAnnotatedType = getAnnotatedType @Text

instance HasAnnotatedInputType Int64 where
  getAnnotatedInputType = getAnnotatedInputType @Int32

instance FromValue Int64 where
  fromValue (ValueInt v) = pure (fromIntegral v)
  fromValue v = wrongType "Int" v

instance HasAnnotatedInputType UUID where
  getAnnotatedInputType = getAnnotatedInputType @Text

instance Defaultable UUID

instance FromValue UUID where
  fromValue (ValueString (String v)) =
    case textToUUID v of
      Nothing -> Left "invalid UUID"
      Just uuid -> pure uuid
  fromValue v = wrongType "String" v

validatedTextToValue :: Validated p Text -> Result Value
validatedTextToValue = Result [] . ValueString . String . getValue

instance forall m p. (Applicative m) => HasResolver m (Validated p Text) where
  type Handler m (Validated p Text) = m (Validated p Text)
  resolve handler Nothing = validatedTextToValue <$> handler
  resolve _ (Just ss) = pure (Result [SubSelectionOnLeaf ss] ValueNull)

instance HasAnnotatedType (Validated p Text) where
  getAnnotatedType = getAnnotatedType @Text

accessTokenToValue :: AccessToken -> Result Value
accessTokenToValue (AccessToken m) = Result [] $ ValueString $ String m

instance forall m. (Applicative m) => HasResolver m AccessToken where
  type Handler m AccessToken = m AccessToken
  resolve handler Nothing = accessTokenToValue <$> handler
  resolve _ (Just ss) = pure (Result [SubSelectionOnLeaf ss] ValueNull)

instance HasAnnotatedType AccessToken where
  getAnnotatedType = getAnnotatedType @Text

utcTimeToValue :: UTCTime -> Result Value
utcTimeToValue m = Result [] $ ValueInt $ floor $ utcTimeToPOSIXSeconds m

instance forall m. (Applicative m) => HasResolver m UTCTime where
  type Handler m UTCTime = m UTCTime
  resolve handler Nothing = utcTimeToValue <$> handler
  resolve _ (Just ss) = pure (Result [SubSelectionOnLeaf ss] ValueNull)

instance HasAnnotatedType UTCTime where
  getAnnotatedType = getAnnotatedType @Int32

emailAddressToValue :: EmailAddress -> Result Value
emailAddressToValue = Result [] . ValueString . String . emailAddressToText

instance forall m. (Applicative m) => HasResolver m EmailAddress where
  type Handler m EmailAddress = m EmailAddress
  resolve handler Nothing = emailAddressToValue <$> handler
  resolve _ (Just ss) = pure (Result [SubSelectionOnLeaf ss] ValueNull)

instance HasAnnotatedType EmailAddress where
  getAnnotatedType = getAnnotatedType @Text
