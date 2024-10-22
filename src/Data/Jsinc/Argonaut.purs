module Data.Jsinc.Argonaut
  ( JAccumulator
  , parseJson
  ) where

import Prelude

import Control.Jsinc.Decoder
  ( class DecodeJsonStream
  , class EndJsonDecode
  , DecodeException(DecodeError)
  , decodeT
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
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
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
import Data.Either (Either(Left, Right))
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data JAccumulator
  = RootAcc
  | StringAcc JAccumulator String
  | ArrayAcc JAccumulator (Array Json)
  | ObjectAcc JAccumulator (Array (Tuple String Json)) (Maybe String)

err ∷ forall m t c a. MonadThrow (DecodeException c a) m ⇒ m t
err = throwError $ DecodeError "Programmatic error: notify the maintainers with the string that was being parsed."

instance MonadThrow (DecodeException JAccumulator Json) m ⇒ EndJsonDecode JAccumulator Json m where
  endJsonDecodeT acc num =
    let f = pure <<< Left
        jVal = fromNumber num
    in
    case acc of
    RootAcc → pure $ Right jVal
    ArrayAcc acc' values → f <<< ArrayAcc acc' $ snoc values jVal
    ObjectAcc acc' props mName →
      maybe
        err
        (\ name → f $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing)
        mName
    _ → err

instance MonadThrow (DecodeException JAccumulator Json) m ⇒ DecodeJsonStream JAccumulator Json m where
  decodeJsonT acc event =
    let f = pure <<< Left
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

parseJson ∷ String → Either (DecodeException JAccumulator Json) Json
parseJson jsonStr = case decodeT jsonStr RootAcc of
  Identity r → r
