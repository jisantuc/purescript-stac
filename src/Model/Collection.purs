module Model.Collection where

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, stringify, toObject)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Foreign.Object (Object, filterKeys)
import Model.Extent (StacExtent)
import Model.Link (StacLink)
import Model.Provider (StacProvider)
import Model.Testing (alphaStringGen, jsObjectGen, maybe)
import Prelude (class Eq, class Show, bind, not, pure, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf)

-- | A `StacCollection` represents a logical grouping of items and other
-- | collections / and catalogs that shares some metadata, namely, temporal
-- | and geographic extents and upstream providers. 
newtype StacCollection
  = StacCollection
  { stacVersion :: String
  , stacExtensions :: Array String
  , id :: String
  , title :: Maybe String
  , description :: String
  , keywords :: Array String
  , license :: String
  , providers :: Array StacProvider
  , extent :: StacExtent
  , summaries :: Json
  , properties :: Json
  , links :: Array StacLink
  , extensionFields :: Object Json
  }

instance decodeJsonStacCollection :: DecodeJson StacCollection where
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
            $ StacCollection
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

instance encodeJsonStacCollection :: EncodeJson StacCollection where
  encodeJson ( StacCollection
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

instance arbitraryStacCollection :: Arbitrary StacCollection where
  arbitrary = do
    stacVersion <- pure $ "1.0.0-beta.2"
    stacExtensions <- pure $ []
    id <- alphaStringGen 12
    title <- maybe (alphaStringGen 12)
    description <- alphaStringGen 12
    keywords <- arrayOf (alphaStringGen 12)
    license <- alphaStringGen 12
    providers <- arrayOf arbitrary
    extent <- (arbitrary :: Gen StacExtent)
    summaries <- encodeJson <$> jsObjectGen
    properties <- encodeJson <$> jsObjectGen
    links <- arrayOf arbitrary
    extensionFields <- jsObjectGen
    pure
      $ StacCollection
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

derive newtype instance eqStacCollection :: Eq StacCollection

instance showStacCollection :: Show StacCollection where
  show = stringify <<< encodeJson
