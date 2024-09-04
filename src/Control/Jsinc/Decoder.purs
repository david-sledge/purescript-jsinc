module Control.Jsinc.Decoder
  ( DecodeExcption(..)
  , class DecodeJsonStream
  , decodeJsonStreamT
  , decodeJsonT
  , decodeStreamT
  , decodeT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , endStreamDecodeT
  )
  where

import Prelude

import Control.Jsinc.Parser
  ( Event
  , ParseException(EOF)
  , ParseState
  , endJsonStreamParseT
  , initParseState
  , parseJsonStreamT
  , runParseT
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (class MonadState, StateT, get, modify, put)
import Data.Either (Either(Left), either)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Source (class Source)
import Data.Tuple (Tuple(Tuple))

data DecodeExcption e
  = DecodeError e
  | ParseError ParseException
  | ShouldNeverHappen

class DecodeJsonStream a e c m where
  decodeJsonT ∷
    MonadThrow (DecodeExcption e) m ⇒
    c →
    Event →
    m (Tuple c (Maybe a))
  endJsonDecodeT ∷
    MonadThrow (DecodeExcption e) m ⇒
    c →
    Event →
    m (Tuple c (Maybe (Maybe a)))

processJsonStreamT ∷ ∀ m a s c ev e.
  MonadState (Tuple s c) m ⇒
  MonadThrow (DecodeExcption e) m ⇒
  (s → m (Tuple (Either ParseException ev) s)) →
  (c → ev → m (Tuple c (Maybe a))) →
  m a
processJsonStreamT f g =
  let recurse = do
        Tuple state acc ← get
        Tuple result state' ← f state
        put (Tuple state' acc)
        either (throwError <<< ParseError) (\ event → do
            (Tuple acc' mRes) ← g acc event
            modify (\ (Tuple state'' _) → Tuple state'' acc') *> maybe recurse pure mRes
          ) result
  in
  recurse

decodeJsonStreamT ∷ ∀ s a e c m.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  MonadThrow (DecodeExcption e) m ⇒
  Source s Char (MaybeT m) ⇒
  DecodeJsonStream a e c m ⇒
  m a
decodeJsonStreamT = processJsonStreamT parseJsonStreamT decodeJsonT

endJsonStreamDecodeT ∷ ∀ s a e c m.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  MonadThrow (DecodeExcption e) m ⇒
  DecodeJsonStream a e c m ⇒
  m (Maybe a)
endJsonStreamDecodeT = processJsonStreamT endJsonStreamParseT endJsonDecodeT

decodeStreamT ∷ ∀ s a c e m.
  DecodeJsonStream a e c (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  Monad m ⇒
  Source s Char (MaybeT (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  Tuple (Tuple ParseState s) c ->
  m (Tuple (Tuple (Maybe a) (DecodeExcption e)) (Tuple (Tuple ParseState s) c))
decodeStreamT state = do
  Tuple result state' ← runParseT decodeJsonStreamT state
  either (\ e -> pure $ Tuple (Tuple Nothing e) state')
    (\ value → do
      -- if a value is returned, the next call...
      Tuple (result' ∷ Either (DecodeExcption e) a) state'' ← runParseT decodeJsonStreamT state'
      either
        -- ... should return an exception...
        (\ e -> pure $ Tuple (Tuple (pure value) e) state'')
        -- ... but not a value.
        (const <<< pure $ Tuple (Tuple Nothing ShouldNeverHappen) state'')
        result'
    ) result

endStreamDecodeT ∷ ∀ s a c e m.
  DecodeJsonStream a e c (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  Monad m ⇒
  Source s Char (MaybeT (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  Tuple (Tuple ParseState s) c ->
  m (Either (DecodeExcption e) (Maybe a))
endStreamDecodeT state = do
  Tuple result _ ← runParseT endJsonStreamDecodeT state
  pure $ either
    Left
    (pure <<< maybe Nothing pure)
    result

decodeT ∷ forall m s c a e.
  DecodeJsonStream a e c (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m)) =>
  Monad m => Source s Char (MaybeT (ExceptT (DecodeExcption e) (StateT (Tuple (Tuple ParseState s) c) m))) =>
  s ->
  c ->
  m (Either (DecodeExcption e) a)
decodeT src c = do
  Tuple (Tuple mA e) state <- decodeStreamT $ Tuple (Tuple initParseState src) c
  case e of
    ParseError EOF →
      endStreamDecodeT state >>=
        pure <<< either
          Left
          (maybe
            (maybe (Left ShouldNeverHappen) pure mA)
            \ a -> maybe (pure a) (const $ Left ShouldNeverHappen) mA
          )
    _ -> pure $ Left e
