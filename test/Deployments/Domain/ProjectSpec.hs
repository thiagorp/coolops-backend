module Deployments.Domain.ProjectSpec where

import RIO
import Test.Hspec

import Database.PostgreSQL.Simple.ToField (ToField(..))

import Deployments.Domain.Project
import Util.Validation

main :: IO ()
main = hspec spec

buildValidName :: Monad m => Text -> m Name
buildValidName name =
  case buildName name of
    Invalid _ -> fail "Invalid name"
    Valid n -> return n

spec :: Spec
spec =
  describe "ToField Name" $
  it "is the same as the one for the text" $ do
    let nameT = "Api Service"
    name <- buildValidName nameT
    show (toField name) `shouldBe` show (toField nameT)
