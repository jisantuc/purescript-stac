module Model.Extent where

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Data.These.Gen (genThese)
import Model.JsonDate (JsonDate)
import Prelude (class Eq, class Show, bind, pure, show, ($), (+), (<$>), (<<<), (>>=))
import Test.QuickCheck (class Arbitrary, arbitrary)

type OneOrBoth a
  = These a a

-- | A TwoDimBbox represents a bounding box in two dimensions.
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

-- | A `SpatialExtent` represents the collection of two dimensional bboxes covered
-- | by a collection.
type SpatialExtent
  = { bbox :: Array TwoDimBbox
    }

-- | A `TemporalExtent` represents the time span covered by a `StacCollection`.
-- | It can be open on no more than one side.
newtype TemporalExtent
  = TemporalExtent (OneOrBoth JsonDate)

derive newtype instance eqTemporalExtent :: Eq TemporalExtent

derive newtype instance showTemporalExtent :: Show TemporalExtent

instance decodeJsonTemporalExtent :: DecodeJson TemporalExtent where
  decodeJson js =
    decodeJson js
      >>= ( case _ of
            [ Just start, Just end ] -> Right <<< TemporalExtent $ Both start end
            [ Just start, Nothing ] -> Right <<< TemporalExtent $ This start
            [ Nothing, Just end ] -> Right <<< TemporalExtent $ That end
            _ -> Left $ UnexpectedValue js
        )

instance encodeJsonTemporalExtent :: EncodeJson TemporalExtent where
  encodeJson (TemporalExtent (This start)) = encodeJson [ Just start, Nothing ]
  encodeJson (TemporalExtent (That end)) = encodeJson [ Nothing, Just end ]
  encodeJson (TemporalExtent (Both start end)) = encodeJson [ start, end ]

instance arbitraryTemporalExtent :: Arbitrary TemporalExtent where
  arbitrary = TemporalExtent <$> genThese arbitrary arbitrary

-- | An `Interval` represents the collection of time spans covered by
-- | a `StacCollection`.
type Interval
  = { interval :: Array TemporalExtent
    }

-- | A `StacExtent` represents the combined time and geographic extents
-- | covered by a `StacCollection`.
type StacExtent
  = { spatial :: SpatialExtent
    , temporal :: Interval
    }
