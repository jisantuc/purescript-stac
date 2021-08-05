module Test.ClientSpec where

import Prelude
import Affjax (printError)
import Client.Stac (getCollections)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Stac (CollectionsResponse(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Expectations about response shapes" do
    it "gets the expected /collections shape"
      $ getCollections "http://localhost:9090"
      >>= ( case _ of
            Left err -> fail ("Didn't decode response successfully" <> (printError err))
            Right (CollectionsResponse { collections }) -> collections `shouldSatisfy` (\x -> length x > 0)
        )
