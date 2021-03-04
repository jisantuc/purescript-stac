module Model.Provider where

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), encodeJson, toString)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Prelude (class Eq, class Show, show, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)

-- | A `ProviderRole` indicates what a provider was responsible for in
-- | the provenance of some data.
data ProviderRole
  = Licensor
  | Producer
  | Processor
  | Host

derive instance eqProviderRole :: Eq ProviderRole

instance showProviderRole :: Show ProviderRole where
  show role = case role of
    Licensor -> "licensor"
    Producer -> "producer"
    Processor -> "processor"
    Host -> "host"

instance decodeProviderRole :: DecodeJson ProviderRole where
  decodeJson js = case toLower <$> toString js of
    Just "licensor" -> Right Licensor
    Just "producer" -> Right Producer
    Just "processor" -> Right Processor
    Just "host" -> Right Host
    Just _ -> Left $ UnexpectedValue js
    Nothing -> Left $ TypeMismatch ("Expected a JSON String")

instance encodeProviderRole :: EncodeJson ProviderRole where
  encodeJson = encodeJson <<< show

instance arbitraryProviderRole :: Arbitrary ProviderRole where
  arbitrary =
    elements
      $ ( Licensor
            `cons'`
              [ Producer
              , Processor
              , Host
              ]
        )

-- | A `Provider` indicates the name and responsibilities of entities
-- | responsible for some piece of the data's provenance. More information
-- | can be found in the [STAC specification](https://github.com/radiantearth/stac-spec/blob/v1.0.0-beta.2/item-spec/common-metadata.md#provider-object).
type Provider
  = { name :: String
    , description :: Maybe String
    , roles :: Array ProviderRole
    , url :: Maybe String
    }
