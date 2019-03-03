{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.HealthCheck
  ( getHealthR
  ) where

import Api.Import

getHealthR :: Handler Value
getHealthR = return $ object ["healthy" .= True]
