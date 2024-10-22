module Web.Fetch.Response.Jsinc where

import Prelude

import Control.Jsinc.Decoder
  ( class DecodeJsonStream
  , class EndJsonDecode
  , DecodeException(DecodeError, ImplementationError, ParseError)
  , decodeContT
  )
import Control.Jsinc.Parser
  ( ParseState
  , describeException
  )
import Control.Monad.Except (ExceptT)
import Control.Monad.Nope (MaybeT)
import Control.Monad.State.Trans (StateT)
import Control.Promise (toAffE)
import Data.Either (Either, either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Source
  ( InPlaceSource
  , LineColumnPosition(LineColumnPosition)
  , SourcePosition(SourcePosition)
  )
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Fetch.Core.Response (Response, body)
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.Streams.ReadableStream (getReader)
import Web.Streams.Reader (read)

decodeJsonResponseStream ∷ ∀ c m.
  MonadAff m ⇒
  DecodeJsonStream c Unit (MaybeT (ExceptT (DecodeException c Unit) (StateT (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c Unit)) m))) ⇒
  EndJsonDecode c Unit (ExceptT (DecodeException c Unit) (StateT (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c Unit)) m)) ⇒
  c →
  Response →
  m Unit
decodeJsonResponseStream initAcc response = do
  reader ← liftEffect $ body response >>= getReader
  decoder ← liftEffect $ new utf8
  liftAff (toAffE (unsafeCoerce $ read reader)) >>=
    maybe (liftEffect $ throw "No data in response") (
        \ chunk → do
          jsonStr ← liftEffect $ decodeWithOptions chunk {stream: true} decoder
          (decodeContT ∷
            String →
            c →
            (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c Unit) → (DecodeException c Unit) → m Unit) →
            (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either c Unit) → (Maybe String → m Unit) → m Unit) →
            (Unit → m Unit) →
            m Unit
          )
            jsonStr
            initAcc
            (\ (Tuple (Tuple _ (SourcePosition _ (LineColumnPosition ndx _ line col))) _) e →
              liftEffect <<< throw $ (
                  case e of
                  ParseError e' -> describeException e'
                  ImplementationError mAcc -> "Type class implementation error: decoder returned " <> (
                      either
                      (\ _ → "an accumulator at the end of parsing the root JSON value instead of unit.")
                      (\ _ → "unit in the middle of parsing the root JSON value instead of an accumulator.")
                      mAcc
                    )
                  DecodeError msg -> "Decode error: " <> msg
                ) <> " At line " <> show (line + 1) <> ", column " <> show (col + 1) <> " (index " <> show ndx <> ")"
            )
            (\ _ f → do
              liftAff (toAffE (unsafeCoerce $ read reader)) >>= maybe
                (f Nothing)
                (\ chunk' → liftEffect (decodeWithOptions chunk' {stream: true} decoder) >>= f <<< Just)
            )
            (\ _ → pure unit)
      )
