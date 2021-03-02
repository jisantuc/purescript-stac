module Model.Item where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), encodeJson, jsonEmptyObject, stringify, toObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Model.Extent (TwoDimBbox)
import Model.Geometry (Geometry)
import Model.ItemAsset (ItemAsset)
import Model.Link (Link)
import Model.Testing (alphaStringGen, jsObjectAGen, jsObjectGen)
import Prelude (class Eq, class Show, apply, bind, map, pure, unit, ($), (<<<), (<>), (>>=))
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype Item
  = Item
  { id :: String
  , stacVersion :: String
  , stacExtensions :: Array String
  , geometry :: Geometry
  , bbox :: TwoDimBbox
  , links :: Array Link
  , assets :: Object ItemAsset
  , collection :: Maybe String
  , properties :: Object Json
  }

derive newtype instance eqItem :: Eq Item

instance showItem :: Show Item where
  show = stringify <<< encodeJson

instance encodeJsonItem :: EncodeJson Item where
  encodeJson (Item obj) =
    "id" := encodeJson obj.id
      ~> "stac_version"
      := encodeJson obj.stacVersion
      ~> "stac_extensions"
      := encodeJson obj.stacExtensions
      ~> "geometry"
      := encodeJson obj.geometry
      ~> "bbox"
      := encodeJson obj.bbox
      ~> "links"
      := encodeJson obj.links
      ~> "assets"
      := encodeJson obj.assets
      ~> "collection"
      := encodeJson obj.collection
      ~> "type"
      := encodeJson "Feature"
      ~> "properties"
      := encodeJson obj.properties
      ~> jsonEmptyObject

instance decodeJsonItem :: DecodeJson Item where
  decodeJson js = case toObject js of
    Just obj -> ado
      _ <-
        obj .: "type"
          >>= ( case _ of
                "Feature" -> Right unit
                s -> Left $ TypeMismatch ("Invalid type for GeoJSON Feature" <> s)
            )
      id <- obj .: "id"
      stacVersion <- obj .: "stac_version"
      stacExtensions <- obj .: "stac_extensions"
      geometry <- obj .: "geometry"
      bbox <- obj .: "bbox"
      links <- obj .: "links"
      assets <- obj .: "assets"
      collection <- obj .: "collection"
      properties <- obj .: "properties"
      in Item
        { id
        , stacVersion
        , stacExtensions
        , geometry
        , bbox
        , links
        , assets
        , collection
        , properties
        }
    Nothing -> Left $ TypeMismatch "Expected JSON object"

instance arbitraryItem :: Arbitrary Item where
  arbitrary = do
    id <- alphaStringGen 10
    stacVersion <- pure "1.0.0-beta2"
    stacExtensions <- pure []
    geometry <- arbitrary
    bbox <- arbitrary
    links <- arbitrary
    assets <- jsObjectAGen
    collection <- arbitrary
    properties <- jsObjectGen
    pure
      $ Item
          { id
          , stacVersion
          , stacExtensions
          , geometry
          , bbox
          , links
          , assets
          , collection
          , properties
          }
