module Model.Collection where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, stringify, toObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Foreign.Object (Object, filterKeys)
import Model.Extent (Extent)
import Model.Link (Link)
import Model.Provider (Provider)
import Model.Testing (alphaStringGen, jsObjectGen, maybe)
import Prelude (class Eq, class Show, bind, not, pure, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf)

-- | A `Collection` represents a logical grouping of items and other
-- | collections / and catalogs that shares some metadata, namely, temporal
-- | and geographic extents and upstream providers.
-- | You can see more in the [STAC specification](https://github.com/radiantearth/stac-spec/blob/v1.0.0-beta.2/collection-spec/collection-spec.md).
newtype Collection
  = Collection
  { stacVersion :: String
  , stacExtensions :: Array String
  , id :: String
  , title :: Maybe String
  , description :: String
  , keywords :: Array String
  , license :: String
  , providers :: Array Provider
  , extent :: Extent
  , summaries :: Json
  , properties :: Json
  , links :: Array Link
  , extensionFields :: Object Json
  }

instance decodeJsonStacCollection :: DecodeJson Collection where
  decodeJson js =
    let
      fields =
        Set.fromFoldable
          [ "stac_version"
          , "stac_extensions"
          , "id"
          , "title"
          , "description"
          , "keywords"
          , "license"
          , "providers"
          , "extent"
          , "summaries"
          , "properties"
          , "links"
          ]
    in
      case toObject js of
        Just jsObject -> do
          stacVersion <- jsObject .: "stac_version"
          stacExtensions <- jsObject .: "stac_extensions"
          id <- jsObject .: "id"
          title <- jsObject .: "title"
          description <- jsObject .: "description"
          keywords <- jsObject .: "keywords"
          license <- jsObject .: "license"
          providers <- jsObject .: "providers"
          extent <- jsObject .: "extent"
          summaries <- jsObject .: "summaries"
          properties <- jsObject .: "properties"
          links <- jsObject .: "links"
          extensionFields <- filterKeys (\key -> not $ elem key fields) <$> decodeJson js
          pure
            $ Collection
                { stacVersion
                , stacExtensions
                , id
                , title
                , description
                , keywords
                , license
                , providers
                , extent
                , summaries
                , properties
                , links
                , extensionFields
                }
        Nothing -> Left $ UnexpectedValue js

instance encodeJsonStacCollection :: EncodeJson Collection where
  encodeJson ( Collection
      { stacVersion
    , stacExtensions
    , id
    , title
    , description
    , keywords
    , license
    , providers
    , extent
    , summaries
    , properties
    , links
    , extensionFields
    }
  ) =
    "stac_version" := stacVersion
      ~> "stac_extensions"
      := stacExtensions
      ~> "id"
      := id
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "keywords"
      := keywords
      ~> "license"
      := license
      ~> "providers"
      := providers
      ~> "extent"
      := extent
      ~> "summaries"
      := summaries
      ~> "properties"
      := properties
      ~> "links"
      := links
      ~> encodeJson extensionFields

instance arbitraryStacCollection :: Arbitrary Collection where
  arbitrary = do
    stacVersion <- pure $ "1.0.0-beta.2"
    stacExtensions <- pure $ []
    id <- alphaStringGen 12
    title <- maybe (alphaStringGen 12)
    description <- alphaStringGen 12
    keywords <- arrayOf (alphaStringGen 12)
    license <- alphaStringGen 12
    providers <- arrayOf arbitrary
    extent <- (arbitrary :: Gen Extent)
    summaries <- encodeJson <$> jsObjectGen
    properties <- encodeJson <$> jsObjectGen
    links <- arrayOf arbitrary
    extensionFields <- jsObjectGen
    pure
      $ Collection
          { stacVersion
          , stacExtensions
          , id
          , title
          , description
          , keywords
          , license
          , providers
          , extent
          , summaries
          , properties
          , links
          , extensionFields
          }

derive newtype instance eqStacCollection :: Eq Collection

instance showStacCollection :: Show Collection where
  show = stringify <<< encodeJson
