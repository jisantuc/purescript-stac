module Test.ClientSpec where

import Prelude
import Affjax (URL, printError)
import Client.Stac (getCollection, getCollectionItem, getCollectionItems, getCollections, getConformance, getLandingPage)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Stac (Collection(..), CollectionsResponse(..), Item(..), LandingPage(..))
import Data.String.NonEmpty (NonEmptyString, toString, unsafeFromString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

unsafeNonEmptyString :: String -> NonEmptyString
unsafeNonEmptyString s = unsafePartial $ unsafeFromString s

stacHost :: URL
stacHost = "http://localhost:9090"

collectionId :: NonEmptyString
collectionId = (unsafeNonEmptyString "landsat-8-l1")

itemId :: NonEmptyString
itemId = unsafeNonEmptyString "LC80140332018166LGN00"

spec :: Spec Unit
spec = do
  describe "Expectations about response shapes" do
    it "fetch collections"
      $ getCollections stacHost
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (CollectionsResponse { collections }) -> collections `shouldSatisfy` (\x -> length x > 0)
        )
    it "fetch an individual collection"
      $ getCollection stacHost collectionId
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (Collection { id }) -> id `shouldEqual` "landsat-8-l1"
        )
    it "fetch collection items"
      $ getCollectionItems stacHost collectionId Nothing
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right { features } -> length features `shouldSatisfy` (_ > 0)
        )
    it "fetch an individual collection item" $ getCollectionItem stacHost collectionId itemId
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (Item { id }) -> id `shouldEqual` (toString itemId)
        )
    it "fetch the landing page" $ getLandingPage stacHost
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right (LandingPage { links }) -> length links `shouldSatisfy` (_ > 0)
        )
    it "fetch conformance" $ getConformance stacHost
      >>= ( case _ of
            Left err -> fail $ "Didn't decode response successfully" <> (printError err)
            Right { conformsTo } -> length conformsTo `shouldSatisfy` (_ > 0)
        )
