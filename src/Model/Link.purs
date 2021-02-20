module Model.Link where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, stringify, toObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Foreign.Object (Object, filterKeys)
import Model.LinkType (LinkType)
import Model.MediaType (MediaType)
import Model.Testing (jsObjectGen)
import Prelude (class Eq, class Show, apply, map, not, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Links from one STAC entity to somewhere else.
-- | "Somewhere else" can be another STAC entity, for example,
-- | a collection linking to a child catalog or an item, or to
-- | somewhere else entirely, for example an item could link
-- | to a specially formatted metadata file.
-- | See the [STAC specification](https://github.com/radiantearth/stac-spec/blob/v1.0.0-beta.2/catalog-spec/catalog-spec.md#link-object)
newtype Link
  = Link
  { href :: String
  , rel :: LinkType
  , _type :: Maybe MediaType
  , title :: Maybe String
  , extensionFields :: Object Json
  }

derive newtype instance eqStacLink :: Eq Link

instance showStacLink :: Show Link where
  show = stringify <<< encodeJson

instance decodeStacLink :: DecodeJson Link where
  decodeJson js = case toObject js of
    Just obj ->
      let
        fields = Set.fromFoldable [ "href", "rel", "type", "title" ]
      in
        ado
          href <- obj .: "href"
          rel <- obj .: "rel"
          _type <- obj .: "type"
          title <- obj .: "title"
          extensionFields <- filterKeys (\key -> not $ elem key fields) <$> decodeJson js
          in Link { href, rel, _type, title, extensionFields }
    Nothing -> Left $ TypeMismatch "Expected a JSON object"

instance encodeJsonStacLink :: EncodeJson Link where
  encodeJson (Link { href, rel, _type, title, extensionFields }) =
    "href" := href
      ~> "rel"
      := rel
      ~> "type"
      := _type
      ~> "title"
      := title
      ~> encodeJson extensionFields

instance arbitraryStacLink :: Arbitrary Link where
  arbitrary = ado
    href <- arbitrary
    rel <- arbitrary
    _type <- arbitrary
    title <- arbitrary
    extensionFields <- jsObjectGen
    in Link { href, rel, _type, title, extensionFields }
