module Common.Config where

import RIO
import RIO.Map as M
import RIO.Process
import RIO.Text (encodeUtf8, unpack)

data PGSettings = PGSettings
  { pgUrl :: !ByteString
  , poolSize :: !Int
  }

envVars :: (MonadIO m) => m EnvVars
envVars = view envVarsL <$> mkDefaultProcessContext

appPort :: (MonadIO m) => m Int
appPort = do
  envPort <- M.lookup "PORT" <$> envVars
  case (envPort >>= return . unpack >>= readMaybe) of
    Nothing -> return 3001
    Just port -> return port

pgSettings :: (MonadIO m) => m PGSettings
pgSettings = do
  envUrl <- M.lookup "DATABASE_URL" <$> envVars
  let pgUrl =
        encodeUtf8
          (fromMaybe "postgresql://localhost/deployment-development" envUrl)
  let poolSize = 10
  return $ PGSettings {..}
