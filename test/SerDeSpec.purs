module Test.SerDeSpec where

import Data.Stac (Asset, AssetRole, Collection, CollectionItemsResponse, CollectionsResponse, ConformanceClasses, Extent, Interval, Item, JsonDate, LandingPage, Link, LinkType, MediaType, Provider, ProviderRole, SpatialExtent, TemporalExtent, TwoDimBbox)
import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (Either(..))
import Test.QuickCheck (class Arbitrary, Result, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec = do
  describe "SerDeSpec" do
    it "TwoDimBbox"
      $ quickCheck (\(x :: TwoDimBbox) -> codecRoundTrip x)
    it "ProviderRole"
      $ quickCheck (\(x :: ProviderRole) -> codecRoundTrip x)
    it "SpatialExtent" $ quickCheck (\(x :: SpatialExtent) -> codecRoundTrip x)
    it "JsonDate" $ quickCheck (\(x :: JsonDate) -> codecRoundTrip x)
    it "TemporalExtent" $ quickCheck (\(x :: TemporalExtent) -> codecRoundTrip x)
    it "Interval" $ quickCheck (\(x :: Interval) -> codecRoundTrip x)
    it "Extent" $ quickCheck (\(x :: Extent) -> codecRoundTrip x)
    it "Provider" $ quickCheck (\(x :: Provider) -> codecRoundTrip x)
    it "LinkType" $ quickCheck (\(x :: LinkType) -> codecRoundTrip x)
    it "Link" $ quickCheck (\(x :: Link) -> codecRoundTrip x)
    it "Collection" $ quickCheck (\(x :: Collection) -> codecRoundTrip x)
    it "CollectionResponse" $ quickCheck (\(x :: CollectionsResponse) -> codecRoundTrip x)
    it "MediaType" $ quickCheck (\(x :: MediaType) -> codecRoundTrip x)
    it "AssetRole" $ quickCheck (\(x :: AssetRole) -> codecRoundTrip x)
    it "Asset" $ quickCheck (\(x :: Asset) -> codecRoundTrip x)
    it "Item" $ quickCheck (\(x :: Item) -> codecRoundTrip x)
    it "CollectionItemsResponse" $ quickCheck (\(x :: CollectionItemsResponse) -> codecRoundTrip x)
    it "LandingPage" $ quickCheck (\(x :: LandingPage) -> codecRoundTrip x)
    it "ConformanceClasses" $ quickCheck (\(x :: ConformanceClasses) -> codecRoundTrip x)

codecRoundTrip :: forall a. Show a => Eq a => Arbitrary a => EncodeJson a => DecodeJson a => a -> Result
codecRoundTrip x = (decodeJson $ encodeJson x) === Right x
