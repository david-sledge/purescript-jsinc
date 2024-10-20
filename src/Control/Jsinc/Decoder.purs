module Control.Jsinc.Decoder
  ( DecodeException(..)
  , class Accumulator
  , class DecodeJsonStream
  , class EndJsonDecode
  , decodeContT
  , decodeJsonStreamT
  , decodeJsonT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , feedMoreData
  , initAcc
  )
  where

import Prelude

import Control.Jsinc.Parser
  ( Event (EJsonEnd)
  , ParseException
  , ParseState
  , endJsonStreamParseT
  , parseJsonStreamT
  , runEndParseT
  , runParseT
  , startState
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Nope (class MonadNuhUh, MaybeT, nope)
import Control.Monad.State (class MonadState, StateT, get, modify, put, runStateT)
import Data.Either (Either(Left), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Show.Generic (genericShow)
import Data.Source (class Source, InPlaceSource, LineColumnPosition, SourcePosition, refillSource)
import Data.Tuple (Tuple(Tuple), fst)
import Debug (trace)

class Accumulator m a where
  initAcc ∷ m a

data DecodeException
  = ImplementationError
  | ParseError ParseException

derive instance eqDecodeException ∷ Eq DecodeException
derive instance genericDecodeException ∷ Generic DecodeException _

instance showDecodeException ∷ Show DecodeException where
  show = genericShow

class MonadThrow DecodeException m ⇐ EndJsonDecode a c m where
  endJsonDecodeT ∷
    c →
    Event →
    m (Either c a)

endJsonStreamDecodeT ∷ ∀ s a c m.
  MonadState (Tuple (Tuple ParseState s) (Either c a)) m ⇒
  MonadThrow DecodeException m ⇒
  EndJsonDecode a c m ⇒
  m a
endJsonStreamDecodeT =
  let recurse = do
        Tuple state mAcc ← get
        Tuple result state' ← endJsonStreamParseT state
        put (Tuple state' mAcc)
        either (throwError <<< ParseError)
          (maybe
              (either (\ c → throwError ImplementationError) pure mAcc)
              (\ event → either (\ acc → do
                    mAcc' ← endJsonDecodeT acc event
                    modify (\ (Tuple state'' _) → Tuple state'' mAcc') *> recurse
                  )
                  (\ _ → throwError ImplementationError)
                  mAcc
              )
          ) result
  in
  recurse

class (Accumulator m c, EndJsonDecode a c m) ⇐ DecodeJsonStream a c m where
  decodeJsonT ∷
    c →
    Event →
    m (Either c a)

decodeJsonStreamT ∷ ∀ m a s c.
  MonadState (Tuple (Tuple ParseState s) (Either c a)) m ⇒
  Source s String Char m ⇒
  MonadThrow DecodeException m ⇒
  MonadNuhUh m ⇒
  DecodeJsonStream a c m ⇒
  m a
decodeJsonStreamT =
  let recurse = do
        Tuple state mAcc ← get
        Tuple result state' ← parseJsonStreamT state
        put (Tuple state' mAcc)
        either (throwError <<< ParseError) (maybe nope (\ event → either (\ acc → do
            mAcc' ← decodeJsonT acc event
            modify (\ (Tuple state'' _) → Tuple state'' mAcc') *>
              either (\ _ → recurse) pure mAcc') (\ _ → throwError ImplementationError) mAcc
          )) result
  in
  recurse

feedMoreData ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
feedMoreData jsonStr (Tuple (Tuple parseState srcState) acc) = do
  srcState' ← refillSource jsonStr srcState
  pure $ Tuple (Tuple parseState srcState') acc

decodeContT ∷ ∀ m a s c r.
  Source s String Char m ⇒
  Accumulator m c ⇒
  Monad m ⇒
  Source s String Char (MaybeT (ExceptT DecodeException (StateT (Tuple (Tuple ParseState s) (Either c a)) m))) ⇒
  DecodeJsonStream a c (MaybeT (ExceptT DecodeException (StateT (Tuple (Tuple ParseState s) (Either c a)) m))) ⇒
  EndJsonDecode a c (ExceptT DecodeException (StateT (Tuple (Tuple ParseState s) (Either c a)) m)) ⇒
  String →
  (Tuple (Tuple ParseState s) (Either c a) → DecodeException → m r) →
  (Tuple (Tuple ParseState s) (Either c a) → (Maybe String → m r) → m r) →
  (a → m r) →
  m r
decodeContT chunk onError onEmpty f = do
  psState ← startState
  state ← feedMoreData chunk <<< Tuple psState <<< Left =<< initAcc
  let recurse state_ = do
        Tuple result state' ← runParseT decodeJsonStreamT state_
        either
          (onError state')
          (\ mA → maybe
            (onEmpty state'
              $ maybe
                -- (f <<< fst =<< runEndParseT endJsonStreamDecodeT state')
                (do
                  Tuple result' state'' ← runEndParseT endJsonStreamDecodeT state'
                  either (onError state'') f result'
                )
                (\ chunk' → recurse =<< feedMoreData chunk' state')
            )
            (\ _ → recurse state')
            mA
          )
          result
  recurse state
