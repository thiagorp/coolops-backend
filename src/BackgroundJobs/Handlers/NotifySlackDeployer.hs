{-# LANGUAGE OverloadedStrings #-}

module BackgroundJobs.Handlers.NotifySlackDeployer
  ( Params(..)
  , call
  ) where

import Import

import Data.Aeson

import BackgroundJobs.Runner
import Slack.UseCases.NotifyUserOfFinishedDeployment hiding (call)
import qualified Slack.UseCases.NotifyUserOfFinishedDeployment as App (call)

data Params =
  Params CompanyId
         UUID

instance FromJSON Params where
  parseJSON = withObject "" $ \o -> Params <$> o .: "company_id" <*> o .: "deployment_id"

instance ToJSON Params where
  toJSON (Params cId dId) = object ["company_id" .= cId, "deployment_id" .= dId]

call :: Params -> App JobReturnType
call (Params cId dId) = do
  r <- App.call cId dId
  case r of
    Left DataNotFound -> finishWithFailure "Data not found"
    Right _ -> finishWithSuccess
