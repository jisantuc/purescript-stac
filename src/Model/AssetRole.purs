module Model.AssetRole where

import Control.Monad.Gen (oneOf)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), encodeJson, toString)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Model.Testing (alphaStringGen)
import Prelude (class Eq, class Ord, class Show, pure, ($), (<$>))
import Test.QuickCheck (class Arbitrary)

-- | An asset role indicates what purpose an item's asset serves.
-- | For instance, an item could have several GeoTiff assets, where
-- | one is the item's data and another is significantly downsampled
-- | copy of the data for use as a thumbnail / preview.
-- | You can see more about asset roles in the
-- | [STAC specification](https://github.com/radiantearth/stac-spec/blob/v1.0.0-beta.2/item-spec/item-spec.md#asset-role-types).
data AssetRole
  = Thumbnail
  | Overview
  | DataRole
  | Metadata
  | VendorAssetRole String

derive instance eqAssetRole :: Eq AssetRole

derive instance ordAssetRole :: Ord AssetRole

derive instance genericAssetRole :: Generic AssetRole _

instance showAssetRole :: Show AssetRole where
  show = genericShow

instance decodeJsonAssetRole :: DecodeJson AssetRole where
  decodeJson js = case toString js of
    Just "thumbnail" -> Right Thumbnail
    Just "overview" -> Right Overview
    Just "data" -> Right DataRole
    Just "metadata" -> Right Metadata
    Just s -> Right $ VendorAssetRole s
    Nothing -> Left $ TypeMismatch "expected a string"

instance encodeJsonAssetRole :: EncodeJson AssetRole where
  encodeJson role = case role of
    Thumbnail -> encodeJson "thumbnail"
    Overview -> encodeJson "overview"
    DataRole -> encodeJson "data"
    Metadata -> encodeJson "metadata"
    VendorAssetRole s -> encodeJson s

instance arbitraryAssetRole :: Arbitrary AssetRole where
  arbitrary =
    oneOf $ toNonEmpty
      $ (VendorAssetRole <$> alphaStringGen 10)
          `cons'`
            ( pure
                <$> [ Thumbnail
                  , Overview
                  , DataRole
                  , Metadata
                  ]
            )
