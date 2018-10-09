module Common.Config where

import RIO
import RIO.Map as M
import RIO.Process
import RIO.Text (encodeUtf8, unpack)

newtype PGSettings = PGSettings
  { pgUrl :: ByteString
  }

envVars :: (MonadIO m) => m EnvVars
envVars = view envVarsL <$> mkDefaultProcessContext

frontendBaseUrl :: (MonadIO m) => m Text
frontendBaseUrl = liftIO $ envVar "FRONTEND_APP_BASE_URL" "http://localhost:3000"

appPort :: (MonadIO m) => m Int
appPort = do
  envPort <- M.lookup "PORT" <$> envVars
  case unpack <$> envPort >>= readMaybe of
    Nothing -> return 3001
    Just port -> return port

slackApiPort :: (MonadIO m) => m Int
slackApiPort = do
  envPort <- M.lookup "SLACK_API_PORT" <$> envVars
  case unpack <$> envPort >>= readMaybe of
    Nothing -> return 3002
    Just port -> return port

pgSettings :: IO PGSettings
pgSettings = do
  pgUrl <- encodeUtf8 <$> envVar "DATABASE_URL" "postgresql://localhost/deployment-development"
  return $ PGSettings {..}

data KubernetesSettings = KubernetesSettings
  { k8sHost :: Text
  , k8sToken :: ByteString
  , k8sNamespace :: ByteString
  }

envVar :: Text -> Text -> IO Text
envVar key defaultValue = do
  maybeValue <- M.lookup key <$> envVars
  return $ fromMaybe defaultValue maybeValue

kubernetesSettings :: IO KubernetesSettings
kubernetesSettings = KubernetesSettings <$> k8sHost_ <*> k8sToken_ <*> k8sNamespace_
  where
    k8sHost_ = envVar "K8S_HOST" "https://192.168.99.100:8443"
    k8sNamespace_ = encodeUtf8 <$> envVar "K8S_NAMESPACE" "default"
    k8sToken_ =
      encodeUtf8 <$>
      envVar
        "K8S_TOKEN"
        "eyJhbGciOiJSUzI1NiIsImtpZCI6IiJ9.eyJpc3MiOiJrdWJlcm5ldGVzL3NlcnZpY2VhY2NvdW50Iiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9uYW1lc3BhY2UiOiJrdWJlLXN5c3RlbSIsImt1YmVybmV0ZXMuaW8vc2VydmljZWFjY291bnQvc2VjcmV0Lm5hbWUiOiJkZWZhdWx0LXRva2VuLXhidm1uIiwia3ViZXJuZXRlcy5pby9zZXJ2aWNlYWNjb3VudC9zZXJ2aWNlLWFjY291bnQubmFtZSI6ImRlZmF1bHQiLCJrdWJlcm5ldGVzLmlvL3NlcnZpY2VhY2NvdW50L3NlcnZpY2UtYWNjb3VudC51aWQiOiI2ZTFlMmE2NS03NTY0LTExZTgtYWM0NS0wODAwMjc0NzBkMjEiLCJzdWIiOiJzeXN0ZW06c2VydmljZWFjY291bnQ6a3ViZS1zeXN0ZW06ZGVmYXVsdCJ9.OG7aJwdmdmmoHG5d2u88XtnPrb4RoYorYPjznVeLpE3VcbxL8xnxm5_oahnKijBxfkG2EWJ6zfH2WJMdYBcKYGUAJ-h7zPzLRnfAWkdaSzhvjGu2I61d0DKnQJJC5tqXDo1z_KRBfBz7osshm0OqXkGLI1Xv2MPwO1VZeZPMYar29ksWgeEtWjzoL51MsLPK8IPrGGZoN5fE-eH96zv4OfW71wTmoTgd_FIVImrlSEUYSvbe_6rpytChhdUAgxEb_69uPox8Q95jcdSqLUxz045I1-njVvg7hVZY5W_V62JGdU19hewcTDvbexorct5MP5mbpEUn-YBHsI5EosOglQ"

data SlackSettings = SlackSettings
  { slackClientId :: !Text
  , slackClientSecret :: !ByteString
  , slackVerificationToken :: !Text
  , slackSigningSecret :: !ByteString
  }

slackSettings :: IO SlackSettings
slackSettings = do
  slackClientId <- envVar "SLACK_CLIENT_ID" "388050218183.406662000402"
  slackClientSecret <- encodeUtf8 <$> envVar "SLACK_CLIENT_SECRET" "8bca4209d59b84e8f341c5e49cc3d26b"
  slackVerificationToken <- envVar "SLACK_VERIFICATION_TOKEN" "AtSllqYHHdWAUpiVuuNfPTU1"
  slackSigningSecret <- encodeUtf8 <$> envVar "SLACK_SIGNING_SECRET" "b2891586cb97af67450b9b85dd6a4295"
  return SlackSettings {..}
