module Data.Jsinc.Argonaut
  ( JAccumulator
  , parseJson
  ) where

import Prelude

import Control.Jsinc.Decoder
  ( class DecodeJsonStream
  , DecodeExcption(DecodeError, ShouldNeverHappen)
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
    , EJsonEnd
    )
  )
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
import Data.Either (Either(Left, Right), either)
import Data.Identity (Identity(Identity))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Source (initStringPosition)
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data JAccumulator
  = RootAcc
  | StringAcc JAccumulator String
  | ArrayAcc JAccumulator (Array Json)
  | ObjectAcc JAccumulator (Array (Tuple String Json)) (Maybe String)

instance DecodeJsonStream Json JAccumulator JAccumulator m where
  decodeJsonT acc event =
    let f acc' = pure $ Tuple acc' Nothing
        err = throwError $ DecodeError acc
        processValue mStr acc' jVal =
          case acc' of
          RootAcc → pure <<< Tuple acc' $ Just jVal
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
    let f acc' = pure $ Tuple acc' Nothing
        g = pure <<< Tuple acc <<< pure
        err = throwError $ DecodeError acc
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

parseJson ∷ String → Either (DecodeExcption JAccumulator) Json
parseJson jsonStr =
  case decodeT (initStringPosition jsonStr) RootAcc of
  Identity (Tuple (Tuple mA mE) parseState) → maybe (maybe (Left ShouldNeverHappen) (Right <<< identity) mA) Left mE
