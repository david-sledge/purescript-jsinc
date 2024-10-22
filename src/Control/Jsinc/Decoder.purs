module Control.Jsinc.Decoder
  ( DecodeException(..)
  , class DecodeJsonStream
  , class EndJsonDecode
  , decodeContT
  , decodeJsonStreamT
  , decodeJsonT
  , decodeT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , feedMoreData
  )
  where

import Prelude

import Control.Jsinc.Parser
  ( Event
  , ParseException
  , ParseState
  , endJsonStreamParseT
  , parseJsonStreamT
  , runEndParseT
  , runParseT
  , startState
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Nope (class MonadNuhUh, MaybeT, nope)
import Control.Monad.State (class MonadState, StateT, get, modify, put)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Source (class Source, InPlaceSource, LineColumnPosition, SourcePosition, refillSource)
import Data.Tuple (Tuple(Tuple))

data DecodeException c e
  = ImplementationError (Either c e)
  | ParseError ParseException
  | DecodeError String

class MonadThrow (DecodeException c a) m ⇐ EndJsonDecode c a m where
  endJsonDecodeT ∷
    c →
    Number →
    m (Either c a)

endJsonStreamDecodeT ∷ ∀ s a c m.
  MonadState (Tuple (Tuple ParseState s) (Either c a)) m ⇒
  MonadThrow (DecodeException c a) m ⇒
  EndJsonDecode c a m ⇒
  m a
endJsonStreamDecodeT =
  let recurse = do
        Tuple state mAcc ← get
        Tuple result state' ← endJsonStreamParseT state
        put (Tuple state' mAcc)
        either (throwError <<< ParseError)
          (maybe
              -- end of JSON string
              (either
                -- should not be an accumulator at this point
                (\ _ → throwError $ ImplementationError mAcc)
                pure
                mAcc
              )
              (\ num → either
                  (\ acc → do
                    mAcc' ← endJsonDecodeT acc num
                    modify (\ (Tuple state'' _) → Tuple state'' mAcc') *> recurse
                  )
                  -- should not have a final decoded value yet
                  (\ _ → throwError $ ImplementationError mAcc)
                  mAcc
              )
          ) result
  in
  recurse

class EndJsonDecode c a m ⇐ DecodeJsonStream c a m where
  decodeJsonT ∷
    c →
    Event →
    m (Either c a)

decodeJsonStreamT ∷ ∀ m a s c.
  MonadState (Tuple (Tuple ParseState s) (Either c a)) m ⇒
  Source s String Char m ⇒
  MonadThrow (DecodeException c a) m ⇒
  MonadNuhUh m ⇒
  DecodeJsonStream c a m ⇒
  m a
decodeJsonStreamT =
  let recurse = do
        Tuple state mAcc ← get
        Tuple result state' ← parseJsonStreamT state
        put (Tuple state' mAcc)
        either
          (throwError <<< ParseError)
          (maybe nope
            (\ event → either
              (\ acc → do
                mAcc' ← decodeJsonT acc event
                modify (\ (Tuple state'' _) → Tuple state'' mAcc') *>
                  either (\ _ → recurse) pure mAcc'
              )
              (\ _ → throwError $ ImplementationError mAcc)
              mAcc
            )
          ) result
  in
  recurse

feedMoreData ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
feedMoreData jsonStr (Tuple (Tuple parseState srcState) acc) = do
  srcState' ← refillSource jsonStr srcState
  pure $ Tuple (Tuple parseState srcState') acc

decodeContT ∷ ∀ m a s c r.
  Source s String Char m ⇒
  Monad m ⇒
  Source s String Char (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple ParseState s) (Either c a)) m))) ⇒
  DecodeJsonStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple ParseState s) (Either c a)) m))) ⇒
  EndJsonDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple ParseState s) (Either c a)) m)) ⇒
  String →
  c →
  (Tuple (Tuple ParseState s) (Either c a) → (DecodeException c a) → m r) →
  (Tuple (Tuple ParseState s) (Either c a) → (Maybe String → m r) → m r) →
  (a → m r) →
  m r
decodeContT chunk initAcc onError onEmpty f = do
  psState ← startState
  state ← feedMoreData chunk <<< Tuple psState $ Left initAcc
  let recurse state_ = do
        Tuple result state' ← runParseT decodeJsonStreamT state_
        either
          (onError state')
          (\ mA → maybe
            (onEmpty state'
              $ maybe
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

decodeT ∷ ∀ m a c.
  Monad m ⇒
  DecodeJsonStream c a (MaybeT (ExceptT (DecodeException c a) (StateT (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m))) ⇒
  EndJsonDecode c a (ExceptT (DecodeException c a) (StateT (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a)) m)) ⇒
  String →
  c →
  m (Either (DecodeException c a) a)
decodeT jsonStr initAcc = (decodeContT ∷
    String →
    c →
    (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (DecodeException c a) → m (Either (DecodeException c a) a)) →
    (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c a) → (Maybe String → m (Either (DecodeException c a) a)) → m (Either (DecodeException c a) a)) →
    (a → m (Either (DecodeException c a) a)) →
    m (Either (DecodeException c a) a))
    jsonStr initAcc (\ _ e → pure $ Left e) (\ _ f → f Nothing) (pure <<< Right)
