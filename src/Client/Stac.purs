module Client.Stac
  ( getCollections
  , getCollection
  , getCollectionItem
  , getCollectionItems
  , nextCollectionItemsPage
  , getConformance
  , getLandingPage
  ) where

import Affjax (Error(..), URL, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty (NonEmptyString, toString)
import Effect.Aff (Aff)
import Model.Collection (Collection)
import Model.CollectionItemsResponse (CollectionItemsResponse)
import Model.CollectionsResponse (CollectionsResponse)
import Model.ConformanceClasses (ConformanceClasses)
import Model.Item (Item)
import Model.LandingPage (LandingPage)
import Model.Link (Link(..))
import Model.LinkType (LinkType(..))
import Prelude (bind, pure, show, ($), (<$>), (<<<), (<>), (==), (>>=))
import URI.Fragment as Fragment

urlSafe :: NonEmptyString -> String
urlSafe = Fragment.unsafeToString <<< Fragment.fromString <<< toString

adaptError :: JsonDecodeError -> Error
adaptError jsErr =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

getDecodedBody :: forall a. DecodeJson a => AX.Response Json -> Either Error a
getDecodedBody =
  lmap adaptError
    <<< decodeJson
    <<< _.body

fetchUrl :: forall a. DecodeJson a => String -> Aff (Either Error a)
fetchUrl urlString = do
  result <-
    AX.request
      $ defaultRequest { url = urlString, responseFormat = ResponseFormat.json }
  pure $ result >>= getDecodedBody

-- | Fetch the `/collections` route from a STAC API.
getCollections :: URL -> Aff (Either Error CollectionsResponse)
getCollections apiHost = fetchUrl $ apiHost <> "/collections"

-- | Fetch a single collection from the `/collections/<id>` route from a STAC API.
-- | This method will URL-encode the collection ID for you, so you're free to provide the
-- | exact value that you'd see, for example, in the response from `getCollections`.
getCollection :: URL -> NonEmptyString -> Aff (Either Error Collection)
getCollection apiHost collectionId = fetchUrl $ apiHost <> "/collections/" <> urlSafe collectionId

-- | Fetch items in a collection from the `/collections/<id>/items` route from a STAC API
-- | This method will URL-encode the collection ID for you, so you're free to provide the
-- | exact value that you'd see, for example, in the response from `getCollections`.
getCollectionItems :: URL -> NonEmptyString -> Maybe Int -> Aff (Either Error CollectionItemsResponse)
getCollectionItems apiHost collectionId limit =
  fetchUrl
    $ apiHost
    <> "/collections/"
    <> urlSafe collectionId
    <> "/items"
    <> ( fromMaybe ""
          $ (("?limit=" <> _) <<< show)
          <$> limit
      )

-- | Fetch a single item from the `/collections/<id>/items/<id>` route from a STAC API.
-- | This method will URL-encode the collection and item IDs for you, so you're free to provide the
-- | exact values that you'd see, for example, in the response from `getCollectionItems`.
getCollectionItem :: URL -> NonEmptyString -> NonEmptyString -> Aff (Either Error Item)
getCollectionItem apiHost collectionId itemId = do
  fetchUrl $ apiHost <> "/collections/" <> urlSafe collectionId <> "/items/" <> urlSafe itemId

-- | Fetch the next page of collection items.
nextCollectionItemsPage :: CollectionItemsResponse -> Aff (Either Error CollectionItemsResponse)
nextCollectionItemsPage { links } = case find (\(Link { rel }) -> rel == Next) links of
  Just (Link { href }) -> fetchUrl href
  Nothing -> pure <<< Right $ { features: [], links: [] }

-- | Fetch the landing page from a STAC api at `/`. This page
-- | holds information about the extensions implemented by this STAC API and
-- | about its conformance classes.
getLandingPage :: URL -> Aff (Either Error LandingPage)
getLandingPage = fetchUrl

-- | Fetch the specifications that this API conforms to.
-- | Note that this informance is also included in the landing page
-- | endpoint, so you probably will never need to call this method specifically.
getConformance :: URL -> Aff (Either Error ConformanceClasses)
getConformance = fetchUrl <<< (_ <> "/conformance")
