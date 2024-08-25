module Control.Monad.Nope
  ( Nope
  , module Control.Monad.Maybe.Trans
  , module Control.Monad.Nope.Class
  , module Control.Monad.Nope.Trans
  , mapNope
  , runNope
  )
  where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Nope.Class (class MonadNope, class MonadNuhUh, attempt, liftMaybe, nope, withResource, yup)
import Control.Monad.Nope.Trans (NopeT, mapNopeT, nopeEither, nopeT, runNopeT)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)

-- | A parametrizable nope monad; computations are either nopes or
-- | pure values. If a computation is noped (see `nope`), it
-- | terminates. Nopes may also be caught with `yup`,
-- | allowing the computation to resume and exit successfully.
-- |
-- | The type parameter `a` is the type of successful results.
-- |
-- | A mechanism for trying many different computations until one succeeds is
-- | provided via the `Alt` instance, specifically the `(<|>)` function.
-- | The first computation to succeed is returned. The `Plus` instance is the same.
type Nope = MaybeT Identity

-- | Run a computation in the `Nope` monad. The inverse of `nope`.
runNope ∷ ∀ a. Nope a → Maybe a
runNope = unwrap <<< runMaybeT

-- | Transform the unwrapped computation using the given function.
mapNope ∷ ∀ a b. (Maybe a → Maybe b) → Nope a → Nope b
mapNope f = mapNopeT (Identity <<< f <<< unwrap)
