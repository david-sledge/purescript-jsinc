module Data.Jsinc.Argonaut
  ( JAccumulator
  , parseJson2
  ) where

import Prelude

import Control.Jsinc.Decoder
  ( class Accumulator
  , class DecodeJsonStream
  , class EndJsonDecode
  , DecodeException(ImplementationError)
  , decodeContT
  )
import Control.Jsinc.Parser
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
    )
  , ParseState
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Nope (MaybeT)
import Control.Monad.State (StateT)
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
import Data.Either (Either(Left, Right), either)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Source (InPlaceSource, LineColumnPosition, SourcePosition)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data JAccumulator
  = RootAcc
  | StringAcc JAccumulator String
  | ArrayAcc JAccumulator (Array Json)
  | ObjectAcc JAccumulator (Array (Tuple String Json)) (Maybe String)

instance Applicative m ⇒ Accumulator m JAccumulator where
  initAcc = pure RootAcc

instance MonadThrow DecodeException m ⇒ EndJsonDecode Json JAccumulator m where
  endJsonDecodeT acc event =
    let f = pure <<< Left
        err = throwError ImplementationError
    in
    case event of
    ENumber num →
      let jVal = fromNumber num in
      case acc of
      RootAcc → pure $ Right jVal
      ArrayAcc acc' values → f <<< ArrayAcc acc' $ snoc values jVal
      ObjectAcc acc' props mName →
        maybe
          err
          (\ name → f $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing)
          mName
      _ → err
    _ → err

instance MonadThrow DecodeException m ⇒ DecodeJsonStream Json JAccumulator m where
  decodeJsonT acc event =
    let f = pure <<< Left
        err = throwError ImplementationError
        processValue mStr acc' jVal =
          case acc' of
          RootAcc → pure $ Right jVal
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
    EStringStart →
      case acc of
      StringAcc _ _ → err
      _ → f $ StringAcc acc ""
    EString str →
      case acc of
      StringAcc acc' str' → f <<< StringAcc acc' $ str' <> str
      _ → err
    EStringEnd →
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
    _ → err

parseJson2 ∷ String → Either DecodeException Json
parseJson2 jsonStr = case (decodeContT ∷
    String →
    (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either JAccumulator Json) → DecodeException → Identity (Either DecodeException Json)) →
    (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either JAccumulator Json) → (Maybe String → Identity (Either DecodeException Json)) → Identity (Either DecodeException Json)) →
    (Json → Identity (Either DecodeException Json)) →
    Identity (Either DecodeException Json))
    jsonStr (\ _ e → pure $ Left e) (\ _ f → f Nothing) (pure <<< Right) of
  Identity r → r
