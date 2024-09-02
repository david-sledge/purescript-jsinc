module Data.Jsinc.Argonaut
  ( JAccumulator
  , parseJson
  ) where

import Prelude

import Control.Jsinc.Decoder (class DecodeJsonStream, DecodeExcption(DecodeError, ParseError), decodeJsonStreamT, endJsonStreamDecodeT)
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
  , ParseException(EOF)
  , initParseState
  , runParseT
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
import Data.Either (Either(Left), either)
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
    let f = pure <<< Left
        err = throwError $ DecodeError acc
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
parseJson jsonStr = case do
  Tuple result state ← runParseT decodeJsonStreamT <<< Tuple (Tuple initParseState $ initStringPosition jsonStr) $ RootAcc
  either (\ e →
      case e of
      ParseError EOF → do
        Tuple result' (Tuple _ acc) ← runParseT endJsonStreamDecodeT state
        either (pure <<< Left) (\ mValue →
            maybe (pure <<< Left $ DecodeError acc) (pure <<< pure) mValue
          ) result'
      _ → pure $ Left e
    )
    (\ value → do
      Tuple (result' ∷ Either (DecodeExcption JAccumulator) Json) (Tuple _ acc) ← runParseT decodeJsonStreamT state
      either (\ e →
          case e of
          ParseError EOF → pure $ pure value
          _ → pure $ Left e
        ) (const <<< pure <<< Left $ DecodeError acc) result'
    ) result of
  Identity result → result
