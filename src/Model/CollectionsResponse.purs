module Model.CollectionsResponse where

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), toObject)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Model.Collection (StacCollection)
import Model.Link (StacLink)
import Prelude (class Eq, class Monoid, class Semigroup, class Show, bind, pure, ($), (<>))
import Test.QuickCheck (class Arbitrary)

newtype CollectionsResponse
  = CollectionsResponse
  { collections :: Array StacCollection
  , links :: Array StacLink
  }

instance semigroupCollectionsResponse :: Semigroup CollectionsResponse where
  append (CollectionsResponse { collections: coll1, links: links1 }) (CollectionsResponse { collections: coll2, links: links2 }) =
    let
      links = links1 <> links2

      collections = coll1 <> coll2
    in
      CollectionsResponse { collections, links }

instance monoidCollectionsResponse :: Monoid CollectionsResponse where
  mempty = CollectionsResponse { collections: mempty, links: mempty }

instance decodeJsonCollectionsResponse :: DecodeJson CollectionsResponse where
  decodeJson js = case toObject js of
    Just obj -> do
      links <- obj .: "links"
      collections <- obj .: "collections"
      pure $ CollectionsResponse { links, collections }
    Nothing -> Left $ TypeMismatch "expected object"

derive newtype instance encodeJsonCollectionsResponse :: EncodeJson CollectionsResponse

derive newtype instance arbitraryCollectionsResponse :: Arbitrary CollectionsResponse

derive newtype instance eqCollectionsResponse :: Eq CollectionsResponse

derive newtype instance showCollectionsResponse :: Show CollectionsResponse
