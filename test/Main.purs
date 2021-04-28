module Test.Main where

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  )
import Data.Either (Either(..))
import Data.Stac
  ( AssetRole
  , Collection
  , CollectionItemsResponse
  , CollectionsResponse
  , ConformanceClasses
  , Extent
  , Interval
  , Item
  , Asset
  , JsonDate
  , LandingPage
  , Link
  , LinkType
  , MediaType
  , Provider
  , ProviderRole
  , SpatialExtent
  , TemporalExtent
  , TwoDimBbox
  )
import Effect (Effect)
import Prelude (class Eq, class Show, Unit, discard, show, (<>), (==))
import Test.QuickCheck (Result, quickCheck, (<?>))

main :: Effect Unit
main = do
  quickCheck (\(x :: TwoDimBbox) -> codecRoundTrip "TwoDimBbox" x)
  quickCheck (\(x :: ProviderRole) -> codecRoundTrip "ProviderRole" x)
  quickCheck (\(x :: SpatialExtent) -> codecRoundTrip "SpatialExtent" x)
  quickCheck (\(x :: JsonDate) -> codecRoundTrip "JsonDate" x)
  quickCheck (\(x :: TemporalExtent) -> codecRoundTrip "TemporalExtent" x)
  quickCheck (\(x :: Interval) -> codecRoundTrip "Interval" x)
  quickCheck (\(x :: Extent) -> codecRoundTrip "Extent" x)
  quickCheck (\(x :: Provider) -> codecRoundTrip "Provider" x)
  quickCheck (\(x :: LinkType) -> codecRoundTrip "LinkType" x)
  quickCheck (\(x :: Link) -> codecRoundTrip "Link" x)
  quickCheck (\(x :: Collection) -> codecRoundTrip "Collection" x)
  quickCheck (\(x :: CollectionsResponse) -> codecRoundTrip "CollectionResponse" x)
  quickCheck (\(x :: MediaType) -> codecRoundTrip "MediaType" x)
  quickCheck (\(x :: AssetRole) -> codecRoundTrip "AssetRole" x)
  quickCheck (\(x :: Asset) -> codecRoundTrip "Asset" x)
  quickCheck (\(x :: Item) -> codecRoundTrip "Item" x)
  quickCheck (\(x :: CollectionItemsResponse) -> codecRoundTrip "CollectionItemsResponse" x)
  quickCheck (\(x :: LandingPage) -> codecRoundTrip "LandingPage" x)
  quickCheck (\(x :: ConformanceClasses) -> codecRoundTrip "ConformanceClasses" x)

codecRoundTrip :: forall a. Eq a => Show a => DecodeJson a => EncodeJson a => String -> a -> Result
codecRoundTrip tag a =
  let
    encoded = encodeJson a

    decoded = decodeJson encoded
  in
    decoded == Right a <?> "Result for " <> tag <> ": \n" <> show decoded <> "\nnot equal to expected:\n" <> show a
