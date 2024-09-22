module Control.Jsinc.Decoder
  ( DecodeExcption(..)
  , class Accumulator
  , class DecodeJsonStream
  , decodeJsonStreamT
  , decodeJsonT
  , decodeLastChunkHelperT
  , decodeLastChunkT
  , decodeT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , feedMoreData
  , initAcc
  )
  where

import Prelude

import Control.Jsinc.Parser
  ( Event
  , ParseException
  , ParseState
  , endJsonStreamParseT
  , initParseState
  , parseJsonStreamT
  , runParseT
  , startState
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Nope (class MonadNuhUh, MaybeT, nope)
import Control.Monad.State (class MonadState, StateT, get, modify, put, runStateT)
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Source (class Source, refillSource)
import Data.Tuple (Tuple(Tuple))

class Accumulator m c where
  initAcc ∷ m c

data DecodeExcption a
  = ImplementationError
  | ParseError ParseException
  | ShouldNeverHappen (Maybe a)

class (Accumulator m c, MonadThrow (DecodeExcption a) m) ⇐ DecodeJsonStream a c m | c → a where
  decodeJsonT ∷
    c →
    Event →
    m (Tuple c (Maybe a))
  endJsonDecodeT ∷
    c →
    Event →
    m (Tuple c (Maybe (Maybe a)))

processJsonStreamT ∷ ∀ m a s c ev a'.
  MonadState (Tuple s c) m ⇒
  MonadThrow (DecodeExcption a') m ⇒
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

decodeJsonStreamT ∷ ∀ m a s a' c.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  Source s String Char m ⇒
  MonadThrow (DecodeExcption a') m ⇒
  MonadNuhUh m ⇒
  DecodeJsonStream a c m ⇒
  m a
decodeJsonStreamT =
  let recurse = do
        Tuple state acc ← get
        Tuple result state' ← parseJsonStreamT state
        put (Tuple state' acc)
        either (throwError <<< ParseError) (maybe nope (\ event → do
            (Tuple acc' mRes) ← decodeJsonT acc event
            modify (\ (Tuple state'' _) → Tuple state'' acc') *> maybe recurse pure mRes
          )) result
  in
  recurse

endJsonStreamDecodeT ∷ ∀ s a a' c m.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  MonadThrow (DecodeExcption a') m ⇒
  DecodeJsonStream a c m ⇒
  m (Maybe a)
endJsonStreamDecodeT = processJsonStreamT endJsonStreamParseT endJsonDecodeT

decodeLastChunkHelperT ∷ ∀ m s a' c a.
  Monad m ⇒
  Source s String Char (MaybeT (ExceptT (DecodeExcption a') (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  DecodeJsonStream a' c (MaybeT (ExceptT (DecodeExcption a') (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  a →
  Tuple (Tuple ParseState s) c →
  m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a'))) (Tuple (Tuple ParseState s) c))
decodeLastChunkHelperT a state = do
  -- When a value has already been suplied, the next call...
  Tuple result state' ← runParseT decodeJsonStreamT state
  pure $ Tuple (Tuple (Just a) $ either
      -- ... should return an exception...
      Just
      (maybe
        -- ... or the end of the chunk
        Nothing
        -- ... but not a value.
        (Just <<< ShouldNeverHappen <<< Just))
      result) state'

-- | Finish decoding a JSON string.
-- decodeLastChunkT ∷ ∀ m s a c a'.
--   Monad m ⇒
--   Source s String Char (MaybeT (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
--   DecodeJsonStream a' c (MaybeT (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
--   DecodeJsonStream a c (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
--   Maybe a →
--   Tuple (Tuple ParseState s) c →
--   m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a))) (Tuple (Tuple ParseState s) c))
decodeLastChunkT mA state = do
  Tuple (Tuple mA' mE) state'' ← maybe
    (do
      Tuple result state' ← runParseT decodeJsonStreamT state
      either
        (\ e → pure $ Tuple (Tuple Nothing $ Just e) state')
        (maybe (pure $ Tuple (Tuple Nothing Nothing) state') (\ a → decodeLastChunkHelperT a state'))
        result
    )
    (\ a → decodeLastChunkHelperT a state)
    mA
  maybe
    (maybe
      (do
        Tuple result state''' ← runStateT (runExceptT endJsonStreamDecodeT) state''
        pure $ (either
            (\ e'' → Tuple (Tuple mA' $ Just e''))
            (\ mA'' → maybe
              (Tuple $ Tuple mA' <<< Just $ ShouldNeverHappen Nothing)
              (const $ Tuple (Tuple mA'' Nothing))
              mA''
            )
            result
          ) state''')
      (const <<< pure $ Tuple (Tuple mA' Nothing) state'')
      mA')
    (\ e' → pure $ Tuple (Tuple mA' $ pure e') state'')
    mE

-- | Decode a JSON string as a single chunk.
decodeT ∷ ∀ m s a c a'.
  Monad m ⇒
  Source s String Char (MaybeT (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  DecodeJsonStream a' c (MaybeT (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m))) ⇒
  DecodeJsonStream a c (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  Accumulator m c ⇒
  s →
  m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a))) (Tuple (Tuple ParseState s) c))
decodeT src = decodeLastChunkT Nothing <<< Tuple (Tuple initParseState src) =<< initAcc

feedMoreData ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
feedMoreData jsonStr (Tuple (Tuple parseState srcState) acc) = do
  srcState' ← refillSource jsonStr srcState
  pure $ Tuple (Tuple parseState srcState') acc

-- decodeContT = 
