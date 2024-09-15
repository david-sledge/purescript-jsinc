module Control.Monad.Nope.Class
  ( attempt
  , class MonadNope
  , class MonadNuhUh
  , liftMaybe
  , nope
  , withResource
  , yup
  )
  where

import Prelude

import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Identity.Trans (IdentityT(IdentityT))
import Control.Monad.Maybe.Trans (MaybeT(MaybeT))
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.State (StateT(StateT))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(WriterT))
import Data.Maybe (Maybe(..), maybe)

-- | The `MonadNuhUh` type class represents those monads which support Nothing via
-- | `nope`, where `nope` halts.
-- |
-- | An implementation is provided for `MaybeT`, (TODO) and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Left zero: `nope >>= h = nope`
-- |
class Monad m ⇐ MonadNuhUh m where
  nope ∷ ∀ a. m a

-- | The `MonadNope` type class represents those monads which support catching
-- | nopes.
-- |
-- | - `yup x h` calls the nope handler `h` if a nope is thrown during the
-- |   evaluation of `x`.
-- |
-- | An implementation is provided for `MaybeT`, and for other monad transformers
-- | defined in this library.
-- |
-- | Laws:
-- |
-- | - Catch: `yup nope h = h`
-- | - Pure: `yup (pure a) h = pure a`
-- |
class MonadNuhUh m ⇐ MonadNope m where
  yup ∷ ∀ a. m a → m a → m a

instance monadNuhUhMaybeT ∷ Monad m ⇒ MonadNuhUh (MaybeT m) where
  nope = MaybeT $ pure Nothing

instance monadNopeMaybeT ∷ Monad m ⇒ MonadNope (MaybeT m) where
  yup (MaybeT m) h = MaybeT (m >>= maybe (case h of MaybeT b → b) (pure <<< Just))

instance monadNuhUhExceptT ∷ MonadNuhUh m ⇒ MonadNuhUh (ExceptT e m) where
  nope = lift nope

instance monadNopeExceptT ∷ MonadNope m ⇒ MonadNope (ExceptT e m) where
  yup (ExceptT m) h =
    ExceptT $ yup m (case h of ExceptT a → a)

instance monadNuhUhIdentityT ∷ MonadNuhUh m ⇒ MonadNuhUh (IdentityT m) where
  nope = lift nope

instance monadNopeIdentityT ∷ MonadNope m ⇒ MonadNope (IdentityT m) where
  yup (IdentityT m) h = IdentityT $ yup m (case h of IdentityT a → a)

instance monadNuhUhReaderT ∷ MonadNuhUh m ⇒ MonadNuhUh (ReaderT r m) where
  nope = lift nope

instance monadNopeReaderT ∷ MonadNope m ⇒ MonadNope (ReaderT r m) where
  yup (ReaderT m) h =
    ReaderT \r → yup (m r) (case h of ReaderT f → f r)

instance monadNuhUhStateT ∷ MonadNuhUh m ⇒ MonadNuhUh (StateT s m) where
  nope = lift nope

instance monadNopeStateT ∷ MonadNope m ⇒ MonadNope (StateT s m) where
  yup (StateT m) h =
    StateT \s → yup (m s) (case h of StateT f → f s)

instance monadNuhUhWriterT ∷ (Monoid w, MonadNuhUh m) ⇒ MonadNuhUh (WriterT w m) where
  nope = lift nope

instance monadNopeWriterT ∷ (Monoid w, MonadNope m) ⇒ MonadNope (WriterT w m) where
  yup (WriterT m) h = WriterT $ yup m (case h of WriterT a → a)

-- | Return `Just` if the given action succeeds, `Nothing` if it nopes.
attempt
  ∷ ∀ m a
   . MonadNope m
  ⇒ m a
  → m (Maybe a)
attempt a = (Just <$> a) `yup` pure Nothing

instance monadNuhUhMaybe ∷ MonadNuhUh Maybe where
  nope = Nothing

instance monadNopeMaybe ∷ MonadNope Maybe where
  yup Nothing h  = h
  yup (Just a) _ = Just a

-- | Make sure that a resource is cleaned up in the event of a nope. The
-- | release action is called regardless of whether the body action nopes or
-- | returns.
withResource
  ∷ ∀ m r a
   . MonadNope m
  ⇒ m r
  → (r → m Unit)
  → (r → m a)
  → m a
withResource acquire release kleisli = do
  resource ← acquire
  result ← attempt $ kleisli resource
  release resource
  maybe nope pure result

-- | Lift a `Maybe` value to a MonadNuhUh monad.
liftMaybe ∷ ∀ m a. MonadNuhUh m ⇒ Maybe a → m a
liftMaybe = maybe nope pure
