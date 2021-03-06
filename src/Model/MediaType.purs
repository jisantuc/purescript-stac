module Model.MediaType where

import Control.Monad.Gen (oneOf)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), encodeJson, toString)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Prelude (class Eq, class Show, pure, ($), (<$>))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Media types tell clients what to expect when they request data
-- | from a link. This type supports some special geo-specific
-- | media types that aren't covered by the existing
-- | [PureScript library](https://pursuit.purescript.org/packages/purescript-media-types/4.0.1/docs/Data.MediaType#t:MediaType)
-- | and adds JSON codecs.
data MediaType
  = GeoTiff
  | COG
  | Jpeg2000
  | PNG
  | JPEG
  | TextXML
  | HTML
  | ApplicationXML
  | JSON
  | PlainText
  | GeoJSON
  | GeoPackage
  | HDF5
  | HDF
  | VendorMediaType String

derive instance eqMediaType :: Eq MediaType

derive instance genericMediaType :: Generic MediaType _

instance showMediaType :: Show MediaType where
  show = genericShow

instance decodeJsonMediaType :: DecodeJson MediaType where
  decodeJson js = case toString js of
    Just "image/tiff; application=geotiff" -> Right GeoTiff
    Just "image/tiff; application=geotiff; profile=cloud-optimized" -> Right COG
    Just "image/jp2" -> Right Jpeg2000
    Just "image/png" -> Right PNG
    Just "image/jpeg" -> Right JPEG
    Just "text/xml" -> Right TextXML
    Just "text/html" -> Right HTML
    Just "application/xml" -> Right ApplicationXML
    Just "application/json" -> Right JSON
    Just "text/plain" -> Right PlainText
    Just "application/geo+json" -> Right GeoJSON
    Just "application/geopackage+sqlite3" -> Right GeoPackage
    Just "application/x-hdf5" -> Right HDF5
    Just "application/x-hdf" -> Right HDF
    Just s -> Right $ VendorMediaType s
    Nothing -> Left $ TypeMismatch "expected a string"

instance encodeJsonMediaType :: EncodeJson MediaType where
  encodeJson mediaType = case mediaType of
    GeoTiff -> encodeJson "image/tiff; application=geotiff"
    COG -> encodeJson "image/tiff; application=geotiff; profile=cloud-optimized"
    Jpeg2000 -> encodeJson "image/jp2"
    PNG -> encodeJson "image/png"
    JPEG -> encodeJson "image/jpeg"
    TextXML -> encodeJson "text/xml"
    HTML -> encodeJson "text/html"
    ApplicationXML -> encodeJson "application/xml"
    JSON -> encodeJson "application/json"
    PlainText -> encodeJson "text/plain"
    GeoJSON -> encodeJson "application/geo+json"
    GeoPackage -> encodeJson "application/geopackage+sqlite3"
    HDF5 -> encodeJson "application/x-hdf5"
    HDF -> encodeJson "application/x-hdf"
    VendorMediaType s -> encodeJson s

instance arbitraryMediaType :: Arbitrary MediaType where
  arbitrary =
    oneOf $ toNonEmpty
      $ (VendorMediaType <$> arbitrary)
          `cons'`
            ( pure
                <$> [ GeoTiff
                  , COG
                  , Jpeg2000
                  , PNG
                  , JPEG
                  , TextXML
                  , HTML
                  , ApplicationXML
                  , JSON
                  , PlainText
                  , GeoJSON
                  , GeoPackage
                  , HDF5
                  , HDF
                  ]
            )
