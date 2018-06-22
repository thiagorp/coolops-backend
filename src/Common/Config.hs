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

pgSettings :: IO PGSettings
pgSettings = do
  pgUrl <-
    encodeUtf8 <$>
    envVar "DATABASE_URL" "postgresql://localhost/deployment-development"
  let poolSize = 10
  return $ PGSettings {..}

data KubernetesSettings = KubernetesSettings
  { k8sHost :: Text
  , k8sToken :: ByteString
  }

envVar :: Text -> Text -> IO Text
envVar key defaultValue = do
  maybeValue <- M.lookup key <$> envVars
  return $ fromMaybe defaultValue maybeValue

kubernetesSettings :: IO KubernetesSettings
kubernetesSettings = KubernetesSettings <$> k8sHost_ <*> k8sToken_
  where
    k8sHost_ = envVar "K8S_HOST" "https://192.168.99.100:8443"
    k8sToken_ =
      encodeUtf8 <$>
      envVar
        "K8s_TOKEN"
        "eyJhbGciOiJSUzI1NiIsImtpZCI6IiJ9.eyJpc3MiOiJrdWJlcm5ldGVzL3NlcnZpY2VhY2NvdW50Iiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9uYW1lc3BhY2UiOiJrdWJlLXN5c3RlbSIsImt1YmVybmV0ZXMuaW8vc2VydmljZWFjY291bnQvc2VjcmV0Lm5hbWUiOiJkZWZhdWx0LXRva2VuLXhidm1uIiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9zZXJ2aWNlLWFjY291bnQubmFtZSI6ImRlZmF1bHQiLCJrdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50L3NlcnZpY2UtYWNjb3VudC51aWQiOiI2ZTFlMmE2NS03NTY0LTExZTgtYWM0NS0wODAwMjc0NzBkMjEiLCJzdWIiOiJzeXN0ZW06c2VydmljZWFjY291bnQ6a3ViZS1zeXN0ZW06ZGVmYXVsdCJ9.OG7aJwdmdmmoHG5d2u88XtnPrb4RoYorYPjznVeLpE3VcbxL8xnxm5_oahnKijBxfkG2EWJ6zfH2WJMdYBcKYGUAJ-h7zPzLRnfAWkdaSzhvjGu2I61d0DKnQJJC5tqXDo1z_KRBfBz7osshm0OqXkGLI1Xv2MPwO1VZeZPMYar29ksWgeEtWjzoL51MsLPK8IPrGGZoN5fE-eH96zv4OfW71wTmoTgd_FIVImrlSEUYSvbe_6rpytChhdUAgxEb_69uPox8Q95jcdSqLUxz045I1-njVvg7hVZY5W_V62JGdU19hewcTDvbexorct5MP5mbpEUn-YBHsI5EosOglQ"
