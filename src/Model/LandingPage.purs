module Model.LandingPage where

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , encodeJson
  , jsonEmptyObject
  , toObject
  , (.:)
  , (:=)
  , (~>)
  )
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Model.Link (Link)
import Model.Testing (alphaStringGen, maybe)
import Prelude (class Eq, class Show, apply, map, pure, ($))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf)

newtype LandingPage
  = LandingPage
  { stacVersion :: String
  , stacExtensions :: Array String
  , title :: Maybe String
  , id :: String
  , description :: String
  , links :: Array Link
  , conformsTo :: Array String
  }

derive newtype instance eqLandingPage :: Eq LandingPage

derive newtype instance showLandingPage :: Show LandingPage

instance arbLandingPage :: Arbitrary LandingPage where
  arbitrary = ado
    stacVersion <- pure "1.0.0-beta2"
    stacExtensions <- arrayOf $ alphaStringGen 10
    title <- maybe $ alphaStringGen 10
    id <- alphaStringGen 10
    description <- alphaStringGen 10
    links <- arbitrary
    conformsTo <- arrayOf $ alphaStringGen 10
    in LandingPage
      { stacVersion
      , stacExtensions
      , title
      , id
      , description
      , links
      , conformsTo
      }

instance encodeJsonLandingPage :: EncodeJson LandingPage where
  encodeJson (LandingPage obj) =
    "id" := encodeJson obj.id
      ~> "stac_version"
      := encodeJson obj.stacVersion
      ~> "stac_extensions"
      := encodeJson obj.stacExtensions
      ~> "title"
      := encodeJson obj.title
      ~> "description"
      := encodeJson obj.description
      ~> "links"
      := encodeJson obj.links
      ~> "conformsTo"
      := encodeJson obj.conformsTo
      ~> jsonEmptyObject

instance decodeJsonLandingPage :: DecodeJson LandingPage where
  decodeJson js = case toObject js of
    Just obj -> ado
      stacVersion <- obj .: "stac_version"
      stacExtensions <- obj .: "stac_extensions"
      title <- obj .: "title"
      description <- obj .: "description"
      links <- obj .: "links"
      conformsTo <- obj .: "conformsTo"
      id <- obj .: "id"
      in LandingPage
        { id
        , stacVersion
        , stacExtensions
        , title
        , description
        , links
        , conformsTo
        }
    Nothing -> Left $ TypeMismatch "Expected JSON object"
