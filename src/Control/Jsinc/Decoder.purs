module Control.Jsinc.Decoder
  ( DecodeExcption(..)
  , class Accumulator
  , class DecodeJsonStream
  , decodeChunkedJsonT
  , decodeJsonStreamT
  , decodeJsonT
  , decodeLastChunkHelperT
  , decodeLastChunkT
  , decodeT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , initAcc
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
import Control.Monad.State (class MonadState, StateT, get, modify, put)
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

decodeJsonStreamT ∷ ∀ s a a' c m.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  MonadThrow (DecodeExcption a') m ⇒
  Source s String Char m ⇒
  DecodeJsonStream a c m ⇒
  m a
decodeJsonStreamT = processJsonStreamT parseJsonStreamT decodeJsonT

endJsonStreamDecodeT ∷ ∀ s a a' c m.
  MonadState (Tuple (Tuple ParseState s) c) m ⇒
  MonadThrow (DecodeExcption a') m ⇒
  DecodeJsonStream a c m ⇒
  m (Maybe a)
endJsonStreamDecodeT = processJsonStreamT endJsonStreamParseT endJsonDecodeT

decodeLastChunkHelperT ∷ ∀ m s a' c a. Monad m => Source s String Char (ExceptT (DecodeExcption a') (StateT (Tuple (Tuple ParseState s) c) m)) => DecodeJsonStream a' c (ExceptT (DecodeExcption a') (StateT (Tuple (Tuple ParseState s) c) m)) => a -> Tuple (Tuple ParseState s) c -> m (Tuple (Tuple (Maybe a) (DecodeExcption a')) (Tuple (Tuple ParseState s) c))
decodeLastChunkHelperT a state = do
  -- When a value has already been supllied, the next call...
  Tuple result state' ← runParseT decodeJsonStreamT state
  pure $ Tuple (Tuple (Just a) $ either
      -- ... should return an exception...
      identity
      -- ... but not a value.
      (ShouldNeverHappen <<< Just)
      result) state'

-- | Finish decoding a JSON string.
decodeLastChunkT ∷ ∀ m s c a.
  Monad m ⇒
  Source s String Char (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  DecodeJsonStream a c (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  Maybe a →
  Tuple (Tuple ParseState s) c →
  m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a))) (Tuple (Tuple ParseState s) c))
decodeLastChunkT mA state = do
  Tuple (Tuple mA' e') state'' ← maybe
    (do
      Tuple result state' ← runParseT decodeJsonStreamT state
      either
        (\ e → pure $ Tuple (Tuple Nothing e) state')
        (\ a → decodeLastChunkHelperT a state'
        ) result
    )
    (\ a → decodeLastChunkHelperT a state)
    mA
  case e' of
    ParseError EOF → maybe
      (do
        Tuple result state''' ← runParseT endJsonStreamDecodeT state''
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
      mA'
    _ → pure $ Tuple (Tuple mA' $ pure e') state''

whuh ∷ ∀ m d c b a s. Bind m ⇒ Source s d c m ⇒ Applicative m ⇒ d → Tuple (Tuple a s) b → m (Tuple (Tuple a s) b)
whuh jsonStr (Tuple (Tuple parseState srcState) acc) = do
  srcState' ← refillSource jsonStr srcState
  pure $ Tuple (Tuple parseState srcState') acc

-- decodeChunkedJsonT ∷ ∀ m d425 c426 s c a. Source s d425 c426 m => Monad m => Source s String Char (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) => DecodeJsonStream a c (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) => s -> c -> m (Maybe String) -> m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a))) (Tuple (Tuple ParseState s) c))
decodeChunkedJsonT initSrc nextChunkF =
  let decodeChunk chunkEndF eofF mA aF state = do
        mChunk ← nextChunkF
        maybe chunkEndF
          (\ chunk → do
            Tuple result state' ← whuh chunk state >>= runParseT decodeJsonStreamT
            either
              (\ e →
                case e of
                ParseError EOF → eofF state'
                _ → pure $ Tuple (Tuple mA $ Just e) state'
              )
              (\ a → aF a state')
              result
          )
          mChunk
      checkRestOfStream a state = decodeChunk
        (do
            Tuple (Tuple mA e) state' ← whuh "" state >>= decodeLastChunkHelperT a
            pure $ Tuple (Tuple mA
                case e of
                ParseError EOF → Nothing
                _ → Just e
              ) state'
        )
        (checkRestOfStream a)
        (Just a)
        (\ a' → pure <<< Tuple (Tuple (Just a) <<< Just <<< ShouldNeverHappen $ Just a'))
        state
      decodeNextChunk state = decodeChunk
        (whuh "" state >>= decodeLastChunkT Nothing)
        decodeNextChunk
        Nothing
        checkRestOfStream
        state
  in
  decodeNextChunk $ Tuple (Tuple initParseState initSrc) initAcc

-- | Decode a JSON string as a single chunk.
decodeT ∷ ∀ m s c a.
  Monad m ⇒
  Accumulator m c ⇒
  Source s String Char (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  DecodeJsonStream a c (ExceptT (DecodeExcption a) (StateT (Tuple (Tuple ParseState s) c) m)) ⇒
  s →
  m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption a))) (Tuple (Tuple ParseState s) c))
decodeT src = decodeLastChunkT Nothing <<< Tuple (Tuple initParseState src) =<< initAcc
