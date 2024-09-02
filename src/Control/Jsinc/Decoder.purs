module Control.Jsinc.Decoder
  ( DecodeExcption(..)
  , class DecodeJsonStream
  , decodeJsonT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , decodeJsonStreamT
  )
  where

import Prelude

import Control.Jsinc.Parser
  ( Event
  , ParseException
  , ParseState
  , endJsonStreamParseT
  , parseJsonStreamT
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (class MonadState, get, modify, put)
import Data.Either (Either, either)
import Data.Maybe (Maybe)
import Data.Source (class Source)
import Data.Tuple (Tuple(Tuple))

data DecodeExcption e
  = DecodeError e
  | ParseError ParseException

class DecodeJsonStream a e c m where
  decodeJsonT ∷ MonadThrow (DecodeExcption e) m ⇒ c → Event → m (Either c a)
  endJsonDecodeT ∷ MonadThrow (DecodeExcption e) m ⇒ c → Event → m (Either c (Maybe a))

processJsonStreamT ∷ ∀ m a s c ev e. MonadState (Tuple s c) m ⇒ MonadThrow (DecodeExcption e) m ⇒ (s → m (Tuple (Either ParseException ev) s)) → (c → ev → m (Either c a)) → m a
processJsonStreamT f g =
  let recurse = do
        Tuple state acc ← get
        Tuple result state' ← f state
        put (Tuple state' acc)
        either (throwError <<< ParseError) (\ event → do
            res ← g acc event
            either (\ acc' → modify (\ (Tuple state'' _) → Tuple state'' acc') *> recurse) pure res
          ) result
  in
  recurse

decodeJsonStreamT ∷ ∀ s a e c m. MonadState (Tuple (Tuple ParseState s) c) m ⇒ MonadThrow (DecodeExcption e) m ⇒ Source s Char (MaybeT m) ⇒ DecodeJsonStream a e c m ⇒ m a
decodeJsonStreamT = processJsonStreamT parseJsonStreamT decodeJsonT

endJsonStreamDecodeT ∷ ∀ s a e c m. MonadState (Tuple (Tuple ParseState s) c) m ⇒ MonadThrow (DecodeExcption e) m ⇒ DecodeJsonStream a e c m ⇒ m (Maybe a)
endJsonStreamDecodeT = processJsonStreamT endJsonStreamParseT endJsonDecodeT
