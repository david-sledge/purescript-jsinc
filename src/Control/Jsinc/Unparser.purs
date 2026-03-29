module Control.Jsinc.Unparser
  ( unparseFalse
  , unparseNull
  , unparseNumber
  , unparseTrue
  )
  where

import Data.Target
import Prelude

import Control.Monad.State (class MonadState, StateT(StateT), get, modify, put, runStateT, state)
import Data.Maybe (maybe)
import Data.Number.Format (toString)
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(Tuple))

pushTargetM ∷ ∀ m c t. MonadState t m ⇒ Target t c m ⇒ c → m Unit
pushTargetM c = get >>= pushTarget c >>= put

unparseNull ∷ ∀ m t. MonadState t m ⇒ Target t Char m ⇒ m Unit
unparseNull = pushTargetM 'n' *> pushTargetM 'u' *> pushTargetM 'l' *> pushTargetM 'l'

unparseFalse ∷ ∀ m t. MonadState t m ⇒ Target t Char m ⇒ m Unit
unparseFalse = pushTargetM 'f' *> pushTargetM 'a' *> pushTargetM 'l' *> pushTargetM 's' *> pushTargetM 'e'

unparseTrue ∷ ∀ m t. MonadState t m ⇒ Target t Char m ⇒ m Unit
unparseTrue = pushTargetM 't' *> pushTargetM 'r' *> pushTargetM 'u' *> pushTargetM 'e'

-- https://tc39.es/ecma262/#sec-numeric-types-number-tostring
unparseNumber ∷ ∀ m t. MonadState t m ⇒ Target t Char m ⇒ Number -> m Unit
unparseNumber num =
  let
    str = toString num
    recurse i = maybe (pure unit) (\ c -> pushTargetM c *> recurse (i + 1)) $ charAt i str
  in
  recurse 0
