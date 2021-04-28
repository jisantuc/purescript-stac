module Codec where

import Data.Argonaut (JsonDecodeError(..))
import Data.Either (Either(..))
import Prelude (class Show, show, ($), (<>))

withPredicate :: forall a. Show a => a -> (a -> Boolean) -> Either JsonDecodeError a
withPredicate v p =
  let
    failureMessage = show v <> " failed a validation"
  in
    if (p v) then Right v else Left $ TypeMismatch failureMessage
