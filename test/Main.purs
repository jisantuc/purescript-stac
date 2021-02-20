module Test.Main where

import Prelude (class Eq, class Show, Unit, discard, show, ($), (<$>), (<*>), (<>), (==))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Date (Month(..))
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Stac
  ( CollectionsResponse
  , Interval
  , JsonDate(..)
  , SpatialExtent
  , StacCollection
  , StacExtent
  , StacLink
  , StacLinkType
  , StacProvider
  , StacProviderRole
  , TemporalExtent
  , TwoDimBbox
  )
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Codec round trips" do
      test "TwoDimBbox" $ liftEffect $ quickCheck (\(x :: TwoDimBbox) -> codecRoundTrip x)
      test "StacProviderRole" $ liftEffect $ quickCheck (\(x :: StacProviderRole) -> codecRoundTrip x)
      test "SpatialExtent" $ liftEffect $ quickCheck (\(x :: SpatialExtent) -> codecRoundTrip x)
      test "JsonDate" $ liftEffect $ quickCheck (\(x :: JsonDate) -> codecRoundTrip x)
      test "TemporalExtent" $ liftEffect $ quickCheck (\(x :: TemporalExtent) -> codecRoundTrip x)
      test "Interval" $ liftEffect $ quickCheck (\(x :: Interval) -> codecRoundTrip x)
      test "StacExtent" $ liftEffect $ quickCheck (\(x :: StacExtent) -> codecRoundTrip x)
      test "StacProvider" $ liftEffect $ quickCheck (\(x :: StacProvider) -> codecRoundTrip x)
      test "StacLinkType" $ liftEffect $ quickCheck (\(x :: StacLinkType) -> codecRoundTrip x)
      test "StacLink" $ liftEffect $ quickCheck (\(x :: StacLink) -> codecRoundTrip x)
      test "StacCollection" $ liftEffect $ quickCheck (\(x :: StacCollection) -> codecRoundTrip x)
      test "StacCollectionResponse" $ liftEffect $ quickCheck (\(x :: CollectionsResponse) -> codecRoundTrip x)

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
