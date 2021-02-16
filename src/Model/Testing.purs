module Model.Testing where

import Prelude
import Control.Apply (lift2)
import Control.Monad.Gen (elements, oneOf)
import Data.Argonaut (Json, encodeJson)
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object.Gen (genForeignObject)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)

alphaStringGen :: Int -> Gen String
alphaStringGen 0 = pure ""

alphaStringGen n =
  let
    choice =
      elements
        $ toNonEmpty
            ( "a" `cons'` [ "b", "c", "d", "e", "f", "g" ]
            )
  in
    lift2 (<>) choice (alphaStringGen (n - 1))

maybe :: forall a. Gen a -> Gen (Maybe a)
maybe g = oneOf $ toNonEmpty $ pure Nothing `cons'` [ Just <$> g ]

jsObjectGen :: Gen (Object Json)
jsObjectGen = genForeignObject (alphaStringGen 12) (encodeJson <$> (arbitrary :: Gen Number))
