module Model.StacLinkType where

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), encodeJson, toString)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Show, pure, ($), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data StacLinkType
  = Self
  | StacRoot
  | Parent
  | Child
  | Item
  | Items
  | Source
  | Collection
  | License
  | Alternate
  | DescribedBy
  | Next
  | Prev
  | ServiceDesc
  | ServiceDoc
  | Conformance
  | Data
  | LatestVersion
  | PredecessorVersion
  | SuccessorVersion
  | DerivedFrom
  | VendorLinkType String

derive instance eqStacLinkType :: Eq StacLinkType

derive instance genericStacLinkType :: Generic StacLinkType _

instance showStacLinkType :: Show StacLinkType where
  show = genericShow

instance decodeStacLinkType :: DecodeJson StacLinkType where
  decodeJson js = case toString js of
    Just "self" -> Right Self
    Just "root" -> Right StacRoot
    Just "parent" -> Right Parent
    Just "child" -> Right Child
    Just "item" -> Right Item
    Just "items" -> Right Items
    Just "source" -> Right Source
    Just "collection" -> Right Collection
    Just "license" -> Right License
    Just "alternate" -> Right Alternate
    Just "describedBy" -> Right DescribedBy
    Just "next" -> Right Next
    Just "prev" -> Right Prev
    Just "service-desc" -> Right ServiceDesc
    Just "service-doc" -> Right ServiceDoc
    Just "conformance" -> Right Conformance
    Just "data" -> Right Data
    Just "latest-version" -> Right LatestVersion
    Just "predecessor-version" -> Right PredecessorVersion
    Just "successor-version" -> Right SuccessorVersion
    Just "derived-from" -> Right DerivedFrom
    Just s -> (Right <<< VendorLinkType) s
    Nothing -> (Left <<< UnexpectedValue) js

instance encodeStacLinkType :: EncodeJson StacLinkType where
  encodeJson stacLinkType =
    encodeJson
      $ case stacLinkType of
          Self -> "self"
          StacRoot -> "root"
          Parent -> "parent"
          Child -> "child"
          Item -> "item"
          Items -> "items"
          Source -> "source"
          Collection -> "collection"
          License -> "license"
          Alternate -> "alternate"
          DescribedBy -> "describedBy"
          Next -> "next"
          Prev -> "prev"
          ServiceDesc -> "service-desc"
          ServiceDoc -> "service-doc"
          Conformance -> "conformance"
          Data -> "data"
          LatestVersion -> "latest-version"
          PredecessorVersion -> "predecessor-version"
          SuccessorVersion -> "successor-version"
          DerivedFrom -> "derived-from"
          VendorLinkType s -> s

instance arbitraryStacLinkType :: Arbitrary StacLinkType where
  arbitrary =
    oneOf
      $ toNonEmpty
      $ (VendorLinkType <$> arbitrary)
          `cons'`
            ( pure
                <$> [ Self
                  , StacRoot
                  , Parent
                  , Child
                  , Item
                  , Items
                  , Source
                  , Collection
                  , License
                  , Alternate
                  , DescribedBy
                  , Next
                  , Prev
                  , ServiceDesc
                  , ServiceDoc
                  , Conformance
                  , Data
                  , LatestVersion
                  , PredecessorVersion
                  , SuccessorVersion
                  , DerivedFrom
                  ]
            )
