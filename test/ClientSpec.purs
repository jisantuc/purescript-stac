module Test.ClientSpec where

import Prelude
import Affjax (printError)
import Client.Stac (getCollection, getCollections)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Stac (Collection(..), CollectionsResponse(..))
import Data.String.NonEmpty (unsafeFromString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  describe "Expectations about response shapes" do
    it "gets the expected /collections shape"
      $ getCollections "http://localhost:9090"
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (CollectionsResponse { collections }) -> collections `shouldSatisfy` (\x -> length x > 0)
        )
    it "fetches an individual collection from /collections/id"
      $ getCollection "http://localhost:9090" (unsafePartial $ unsafeFromString "landsat-8-l1")
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (Collection { id }) -> id `shouldEqual` "landsat-8-l1"
        )
