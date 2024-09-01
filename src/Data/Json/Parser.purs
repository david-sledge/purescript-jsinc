module Data.Json.Parser
  ( Accumulator(..)
  , DecodeExcption(..)
  , class DecodeJsonStream
  , decodeJsonT
  , endJsonDecodeT
  , endJsonStreamDecodeT
  , parseJson
  , decodeJsonStreamT
  )
  where

import Prelude

import Control.Json.Core.Parser
  ( Event
    ( EBool
    , ENumber
    , ENull
    , EString
    , EStringStart
    , EStringEnd
    , EArrayStart
    , EArrayEnd
    , EObjectStart
    , EObjectEnd
    , EJsonEnd
    )
  , ParseException(EOF, FlogTheDeveloper, DataAfterJson)
  , ParseState
  , emptyStartState
  , endJsonStreamParseT
  , initParseState
  , parseJsonStreamT
  , runParseT
  , startState
  , stateString
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (class MonadState, get, modify, put)
import Data.Argonaut
  ( Json
  , fromArray
  , fromBoolean
  , fromNumber
  , fromObject
  , fromString
  , jsonNull
  )
import Data.Array (snoc)
import Data.Either (Either(Left), either)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Source
  ( class Source
  , initStringPosition
  )
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data DecodeExcption e
  = ProgrammaticError e
  | ParseError ParseException

class DecodeJsonStream a c where
  decodeJsonT ∷ ∀ m. MonadThrow (DecodeExcption c) m ⇒ c → Event → m (Either c a)
  endJsonDecodeT ∷ ∀ m. MonadThrow (DecodeExcption c) m ⇒ c → Event → m (Either c (Maybe a))

processJsonStreamT ∷ ∀ m a s c' ev c. MonadState (Tuple s c') m ⇒ MonadThrow (DecodeExcption c) m ⇒ (s → m (Tuple (Either ParseException ev) s)) → (c' → ev → m (Either c' a)) → m a
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

decodeJsonStreamT ∷ ∀ m a c s. MonadState (Tuple (Tuple ParseState s) c) m ⇒ MonadThrow (DecodeExcption c) m ⇒ Source s Char (MaybeT m) ⇒ DecodeJsonStream a c ⇒ m a
decodeJsonStreamT = processJsonStreamT parseJsonStreamT decodeJsonT

endJsonStreamDecodeT ∷ ∀ m c s a. MonadState (Tuple (Tuple ParseState s) c) m ⇒ MonadThrow (DecodeExcption c) m ⇒ DecodeJsonStream a c ⇒ m (Maybe a)
endJsonStreamDecodeT = processJsonStreamT endJsonStreamParseT endJsonDecodeT

data Accumulator
  = RootAcc
  | StringAcc Accumulator String
  | ArrayAcc Accumulator (Array Json)
  | ObjectAcc Accumulator (Array (Tuple String Json)) (Maybe String)

instance DecodeJsonStream Json Accumulator where
  decodeJsonT acc event =
    let f = pure <<< Left
        err = throwError $ ProgrammaticError acc
        processValue mStr acc' jVal =
          case acc' of
            RootAcc → pure $ pure jVal
            ArrayAcc acc'' values → f <<< ArrayAcc acc'' $ snoc values jVal
            ObjectAcc acc'' props mName →
              maybe
                (maybe err (\ str → f <<< ObjectAcc acc'' props $ Just str) mStr)
                (\ name → f $ ObjectAcc acc'' (snoc props $ Tuple name jVal) Nothing)
                mName
            _ → err
    in
    case event of
      ENumber num → processValue Nothing acc $ fromNumber num
      ENull → processValue Nothing acc jsonNull
      EBool bool → processValue Nothing acc $ fromBoolean bool
      EStringStart _ →
        case acc of
          StringAcc _ _ → err
          _ → f $ StringAcc acc ""
      EString _ str →
        case acc of
          StringAcc acc' str' → f <<< StringAcc acc' $ str' <> str
          _ → err
      EStringEnd _ →
        case acc of
          StringAcc acc' str → processValue (Just str) acc' $ fromString str
          _ → err
      EArrayStart → f $ ArrayAcc acc []
      EArrayEnd →
        case acc of
          ArrayAcc acc' elems → processValue Nothing acc' $ fromArray elems
          _ → err
      EObjectStart → f $ ObjectAcc acc [] Nothing
      EObjectEnd →
        case acc of
          ObjectAcc acc' props mName →
            case mName of
              Nothing → processValue Nothing acc' <<< fromObject $ fromFoldable props
              Just _ → err
          _ → err
      EJsonEnd → err

  endJsonDecodeT acc event =
    let f = pure <<< Left
        g = pure <<< pure
        err = throwError $ ProgrammaticError acc
    in
    case event of
      ENumber num →
        let jVal = fromNumber num in
        case acc of
          RootAcc → g $ Just jVal
          ArrayAcc acc' values → f <<< ArrayAcc acc' $ snoc values jVal
          ObjectAcc acc' props mName →
            maybe
              err
              (\ name → f $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing)
              mName
          _ → err
      EJsonEnd →
        case acc of
          RootAcc → g Nothing
          _ → err
      _ → err

parseJson ∷ String → Either (DecodeExcption Accumulator) Json
parseJson jsonStr = case do
  Tuple result state ← runParseT decodeJsonStreamT <<< Tuple (Tuple initParseState $ initStringPosition jsonStr) $ RootAcc
  either (\ e →
      case e of
      ParseError EOF → do
        Tuple result' (Tuple _ acc) ← runParseT endJsonStreamDecodeT state
        either (pure <<< Left) (\ mValue →
            maybe (pure <<< Left $ ProgrammaticError acc) (pure <<< pure) mValue
          ) result'
      _ → pure $ Left e
    )
    (\ value → do
      Tuple (result' ∷ Either (DecodeExcption Accumulator) Json) (Tuple _ acc) ← runParseT decodeJsonStreamT state
      either (\ e →
          case e of
          ParseError EOF → pure $ pure value
          _ → pure $ Left e
        ) (const <<< pure <<< Left $ ProgrammaticError acc) result'
    ) result of
  Identity result → result
