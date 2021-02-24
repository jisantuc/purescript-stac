module Model.ItemAsset where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, stringify, toObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Foreign.Object (Object, filterKeys)
import Model.AssetRole (AssetRole)
import Model.MediaType (MediaType)
import Model.Testing (alphaStringGen, jsObjectGen, maybe)
import Prelude (class Eq, class Show, apply, map, not, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (listOf)

-- | An asset represents some external file that provides data for this item.
-- | Some examples of assets are a COG that holds the item's data and a
-- | GeoJSON collection of labels for a label item.
-- | You can see more in the
-- | [STAC specification](https://github.com/radiantearth/stac-spec/blob/v1.0.0-beta.2/item-spec/item-spec.md#asset-object).
newtype ItemAsset
  = ItemAsset
  { href :: String
  , title :: Maybe String
  , description :: Maybe String
  , roles :: Set.Set AssetRole
  , _type :: Maybe MediaType
  , extensionFields :: Object Json
  }

derive newtype instance eqItemAsset :: Eq ItemAsset

derive instance genericItemAsset :: Generic ItemAsset _

instance showItemAsset :: Show ItemAsset where
  show = stringify <<< encodeJson

instance decodeJsonItemAsset :: DecodeJson ItemAsset where
  decodeJson js =
    let
      fields = Set.fromFoldable [ "href", "title", "type", "description", "roles" ]
    in
      case toObject js of
        Just obj -> ado
          href <- obj .: "href"
          title <- obj .: "title"
          _type <- obj .: "type"
          description <- obj .: "description"
          roles <- obj .: "roles"
          extensionFields <- filterKeys (\key -> not $ elem key fields) <$> decodeJson js
          in ItemAsset { href, title, _type, description, roles, extensionFields }
        Nothing -> Left $ TypeMismatch "expected object"

instance encodeJsonItemAsset :: EncodeJson ItemAsset where
  encodeJson (ItemAsset { href, title, description, roles, _type, extensionFields }) =
    "href" := href
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "type"
      := _type
      ~> "roles"
      := roles
      ~> encodeJson extensionFields

instance arbitraryItemAsset :: Arbitrary ItemAsset where
  arbitrary = ado
    href <- alphaStringGen 10
    title <- maybe $ alphaStringGen 10
    description <- maybe $ alphaStringGen 10
    roles <- Set.fromFoldable <$> listOf 3 arbitrary
    extensionFields <- jsObjectGen
    _type <- arbitrary
    in ItemAsset { href, title, description, roles, extensionFields, _type }
