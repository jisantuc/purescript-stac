module Client.Stac (getCollections) where

import Affjax (Error(..), defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, fromAff)
import Data.Argonaut (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect (Effect)
import Model.CollectionsResponse (CollectionsResponse)
import Prelude (bind, pure, ($), (<>), (>>=))

adaptError :: JsonDecodeError -> Error
adaptError jsErr =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

-- | Fetch the `/collections` route from a STAC API.
getCollections :: String -> Effect (Promise (Either Error CollectionsResponse))
getCollections apiHost =
  fromAff
    $ do
        result <-
          AX.request
            $ defaultRequest { url = apiHost <> "/collections", responseFormat = ResponseFormat.json }
        pure $ result
          >>= ( \response ->
                lmap adaptError $ decodeCollections response.body
            )
  where
  decodeCollections js = decodeJson js :: Either JsonDecodeError CollectionsResponse
