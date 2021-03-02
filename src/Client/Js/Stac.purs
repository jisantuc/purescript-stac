module Client.Js.Stac where

import Affjax (Error, URL)
import Client.Stac as Stac
import Control.Promise (Promise, fromAff)
import Data.Either (Either)
import Data.String.NonEmpty (NonEmptyString)
import Effect (Effect)
import Model.Collection (Collection)
import Model.CollectionItemsResponse (CollectionItemsResponse)
import Model.CollectionsResponse (CollectionsResponse)
import Model.Item (Item)
import Model.LandingPage (LandingPage)
import Prelude (($), (<<<))

-- | Fetch the `/collections` route from a STAC API.
getCollections :: URL -> Effect (Promise (Either Error CollectionsResponse))
getCollections = fromAff <<< Stac.getCollections

-- | Fetch items in a collection from the `/collections/<id>/items` route from a STAC API
-- | This method will URL-encode the collection ID for you, so you're free to provide the
-- | exact value that you'd see, for example, in the response from `getCollections`.
getCollectionItems :: URL -> NonEmptyString -> Effect (Promise (Either Error CollectionItemsResponse))
getCollectionItems apiHost collectionId = fromAff $ Stac.getCollectionItems apiHost collectionId

-- | Fetch a single item from the `/collections/<id>/items/<id>` route from a STAC API
-- | This method will URL-encode the collection and item IDs for you, so you're free to provide the
-- | exact values that you'd see, for example, in the response from `getCollectionItems`.
getCollectionItem :: URL -> NonEmptyString -> NonEmptyString -> Effect (Promise (Either Error Item))
getCollectionItem apiHost collectionId itemId = fromAff $ Stac.getCollectionItem apiHost collectionId itemId

-- | Fetch a single collection from the `/collections/<id>` route from a STAC API.
-- | This method will URL-encode the collection ID for you, so you're free to provide the
-- | exact value that you'd see, for example, in the response from `getCollections`.
getCollection :: URL -> NonEmptyString -> Effect (Promise (Either Error Collection))
getCollection apiHost collectionId = fromAff $ Stac.getCollection apiHost collectionId

-- | Fetch the next page of collection items.
nextCollectionItemsPage :: CollectionItemsResponse -> Effect (Promise (Either Error CollectionItemsResponse))
nextCollectionItemsPage = fromAff <<< Stac.nextCollectionItemsPage

-- | Fetch the landing page from a STAC api at `/`. This page
-- | holds information about the extensions implemented by this STAC API and
-- | about its conformance classes.
getLandingPage :: URL -> Effect (Promise (Either Error LandingPage))
getLandingPage = fromAff <<< Stac.getLandingPage
