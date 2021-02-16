module Model.Extent where

import Control.Monad.Gen (oneOf)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Maybe (Maybe(..))
import Data.Refined (class Predicate, Refined, RefinedError(..), unsafeRefine)
import Data.Typelevel.Bool (class Bool)
import Model.JsonDate (JsonDate)
import Prelude (class Eq, class Show, bind, pure, show, ($), (+), (<$>), (>>=))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- predicate requiring at least one non-Nothing item in a list of two items
-- implies SizeEqualTo D2, but I don't know how to tell the compiler that
data OneOrBoth a

instance predicateOneOrBoth :: Predicate (OneOrBoth p) (Array (Maybe x)) where
  validate _ arr = case arr of
    [ Just _, _ ] -> Right arr
    [ _, Just _ ] -> Right arr
    _ -> Left NotError

newtype TwoDimBbox
  = TwoDimBbox
  { llx :: Number
  , lly :: Number
  , urx :: Number
  , ury :: Number
  }

derive instance eqTwoDimBbox :: Eq TwoDimBbox

instance showTwoDimBbox :: Show TwoDimBbox where
  show (TwoDimBbox { llx, lly, urx, ury }) = show [ llx, lly, urx, ury ]

instance decodeTwoDimBbox :: DecodeJson TwoDimBbox where
  decodeJson js =
    decodeJson js
      >>= ( \x -> case x of
            [ llx, lly, urx, ury ] -> Right $ TwoDimBbox { llx, lly, urx, ury }
            _ -> Left $ UnexpectedValue js
        )

instance encodeTwoDimBbox :: EncodeJson TwoDimBbox where
  encodeJson (TwoDimBbox { llx, lly, urx, ury }) = encodeJson [ llx, lly, urx, ury ]

instance arbitraryTwoDimBbox :: Arbitrary TwoDimBbox where
  arbitrary = do
    llx <- arbitrary
    lly <- arbitrary
    let
      urx = llx + 5.0

      ury = lly + 5.0
    pure $ TwoDimBbox { llx, lly, urx, ury }

type SpatialExtent
  = { bbox :: Array TwoDimBbox
    }

-- TODO: handwrite json and arbitrary instances, derive eq and show
newtype TemporalExtent
  = TemporalExtent (Refined (OneOrBoth JsonDate) (Array (Maybe JsonDate)))

derive newtype instance eqTemporalExtent :: Eq TemporalExtent

derive newtype instance showTemporalExtent :: Show TemporalExtent

instance decodeJsonTemporalExtent :: DecodeJson TemporalExtent where
  decodeJson js = TemporalExtent <$> decodeJson js

derive newtype instance encodeJsonTemporalExtent :: EncodeJson TemporalExtent

instance arbitraryTemporalExtent :: Arbitrary TemporalExtent where
  arbitrary = oneOf $ toNonEmpty $ emptyStartGen `cons'` [ emptyEndGen, bothEndpoints ]
    where
    emptyStartGen = do
      start <- pure Nothing
      end <- Just <$> arbitrary
      pure $ TemporalExtent (unsafeRefine [ start, end ])

    emptyEndGen = do
      start <- Just <$> arbitrary
      end <- pure Nothing
      pure $ TemporalExtent (unsafeRefine [ start, end ])

    bothEndpoints = do
      start <- Just <$> arbitrary
      end <- Just <$> arbitrary
      pure $ TemporalExtent (unsafeRefine [ start, end ])

type Interval
  = { interval :: Array TemporalExtent
    }

type StacExtent
  = { spatial :: SpatialExtent
    , temporal :: Interval
    }

type Foo
  = { x :: Int
    , y :: Int
    }
