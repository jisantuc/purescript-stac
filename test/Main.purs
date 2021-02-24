module Test.Main where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Date (Month(..))
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Stac
  ( AssetRole
  , Collection
  , CollectionsResponse
  , Extent
  , Interval
  , Item
  , ItemAsset
  , JsonDate(..)
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
import Effect.Class (liftEffect)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (<$>), (<*>), (<>), (==))
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Codec round trips" do
      test "TwoDimBbox" $ liftEffect $ quickCheck (\(x :: TwoDimBbox) -> codecRoundTrip x)
      test "ProviderRole" $ liftEffect $ quickCheck (\(x :: ProviderRole) -> codecRoundTrip x)
      test "SpatialExtent" $ liftEffect $ quickCheck (\(x :: SpatialExtent) -> codecRoundTrip x)
      test "JsonDate" $ liftEffect $ quickCheck (\(x :: JsonDate) -> codecRoundTrip x)
      test "TemporalExtent" $ liftEffect $ quickCheck (\(x :: TemporalExtent) -> codecRoundTrip x)
      test "Interval" $ liftEffect $ quickCheck (\(x :: Interval) -> codecRoundTrip x)
      test "Extent" $ liftEffect $ quickCheck (\(x :: Extent) -> codecRoundTrip x)
      test "Provider" $ liftEffect $ quickCheck (\(x :: Provider) -> codecRoundTrip x)
      test "LinkType" $ liftEffect $ quickCheck (\(x :: LinkType) -> codecRoundTrip x)
      test "Link" $ liftEffect $ quickCheck (\(x :: Link) -> codecRoundTrip x)
      test "Collection" $ liftEffect $ quickCheck (\(x :: Collection) -> codecRoundTrip x)
      test "CollectionResponse" $ liftEffect $ quickCheck (\(x :: CollectionsResponse) -> codecRoundTrip x)
      test "MediaType" $ liftEffect $ quickCheck (\(x :: MediaType) -> codecRoundTrip x)
      test "AssetRole" $ liftEffect $ quickCheck (\(x :: AssetRole) -> codecRoundTrip x)
      test "ItemAsset" $ liftEffect $ quickCheck (\(x :: ItemAsset) -> codecRoundTrip x)
      test "Item" $ liftEffect $ quickCheck (\(x :: Item) -> codecRoundTrip x)

dateTime :: Maybe JsonDate
dateTime =
  let
    date = canonicalDate <$> toEnum 2021 <*> Just January <*> toEnum 1

    time = Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
  in
    JsonDate <$> (DateTime <$> date <*> time)

codecRoundTrip :: forall a. Eq a => Show a => DecodeJson a => EncodeJson a => a -> Result
codecRoundTrip a =
  let
    encoded = encodeJson a

    decoded = decodeJson encoded
  in
    decoded == Right a <?> "Result: \n" <> show decoded <> "\nnot equal to expected:\n" <> show a
