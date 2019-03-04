{-# LANGUAGE OverloadedStrings #-}

module Util.RequestLogger (jsonLogger) where

import RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.HashMap as HM

import Data.Aeson

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON

jsonLogger :: OutputFormatterWithDetails
jsonLogger date req status responseSize duration reqBody =
  formatAsJSON date req status responseSize duration (removeSensitiveInformation reqBody)

removeSensitiveInformation :: [ByteString] -> [ByteString]
removeSensitiveInformation body = [ removePassword . BS.concat $ body ]

removePassword :: ByteString -> ByteString
removePassword body =
  case decodeStrict body of
    Just (Object jsonBody) ->
      LBS.toStrict . encode $ HM.mapWithKey hideIfPassword jsonBody

    _ ->
      body

hideIfPassword :: Text -> Value -> Value
hideIfPassword "password" _ = String "[Password]"
hideIfPassword _ v = v
