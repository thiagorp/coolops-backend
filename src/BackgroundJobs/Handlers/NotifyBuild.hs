{-# LANGUAGE OverloadedStrings #-}

module BackgroundJobs.Handlers.NotifyBuild
  ( Params(..)
  , call
  ) where

import Import

import Data.Aeson

import BackgroundJobs.Runner
import Slack.UseCases.NotifyNewBuild hiding (call)
import qualified Slack.UseCases.NotifyNewBuild as App (call)

data Params = Params CompanyId UUID

instance FromJSON Params where
  parseJSON = withObject "" $ \o -> Params <$> o .: "company_id" <*> o .: "build_id"

instance ToJSON Params where
  toJSON (Params cId bId) = object ["company_id" .= cId, "build_id" .= bId]

call :: Params -> App JobReturnType
call (Params cId bId) = do
  r <- App.call cId bId
  case r of
    Left MessageDataNotFound -> finishWithFailure "Message data not found"
    Left BuildNotFound -> finishWithFailure "Build not found"
    Left SlackConfigNotFound -> finishWithFailure "Slack config not found"
    Left SlackAccessTokenNotFound -> finishWithFailure "Slack access token not found"
    Right _ -> finishWithSuccess
