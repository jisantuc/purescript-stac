module Model.Geometry where

import Control.Alternative ((<|>))
import Control.Monad.Gen (oneOf)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Prelude (class Eq, ($), (<$>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Turf.Helpers (MultiPolygonGeom, PolygonGeom)

data Geometry
  = Polygon PolygonGeom
  | MultiPolygon MultiPolygonGeom

derive instance eqGeometry :: Eq Geometry

instance decodeJson :: DecodeJson Geometry where
  decodeJson js = Polygon <$> decodeJson js <|> MultiPolygon <$> decodeJson js

instance encodeJson :: EncodeJson Geometry where
  encodeJson (Polygon pGeom) = encodeJson pGeom
  encodeJson (MultiPolygon mpGeom) = encodeJson mpGeom

instance arbitraryGeometry :: Arbitrary Geometry where
  arbitrary =
    oneOf $ toNonEmpty
      $ (Polygon <$> arbitrary)
          `cons'`
            [ MultiPolygon <$> arbitrary ]
