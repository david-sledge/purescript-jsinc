module Data.Json.Parser
  ( parseEmbeddedJson
  , parseJson
  )
  where

import Prelude

import Control.Json.Parser
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
  , ParseException(EOF, Msg, FlogTheDeveloper, DataAfterJson)
  , emptyStartState
  , endParseT
  , parseJsonT
  , parseNextJsonValueT
  , stateString
  )
import Control.Monad.Except (runExceptT, throwError)
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
  ( LineColumnPosition(LineColumnPosition)
  , SourcePosition(SourcePosition)
  )
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data Accumulator
  = RootAcc (Maybe Json)
  | StringAcc Accumulator String
  | ArrayAcc Accumulator (Array Json)
  | ObjectAcc Accumulator (Array (Tuple String Json)) (Maybe String)

_parseJson ∷ Boolean → String → Either String Json
_parseJson isEmbedded jsonStr =
  let parseNext state acc =
        let meh (Tuple result state'@(Tuple _ (SourcePosition _ (LineColumnPosition pos _ line col)))) =
              either (\ e →
                  case e of
                    EOF → endParseT state' >>= meh
                    Msg msg → throwError $ "Error: " <> msg
                    FlogTheDeveloper _ → throwError "Whoa! Hol' up!" 
                    DataAfterJson → throwError
                        if isEmbedded
                        then "Programmatic error!"
                        else "SyntaxError: Unexpected non-whitespace character after JSON at position " <> show pos <> " (line " <> show (line + 1) <> " column " <> show (col + 1) <> ")"
                ) (\ event →
                  let processRootValue mVal jVal =
                        case mVal of
                          Nothing → if isEmbedded then pure jVal else parseNext state' <<< RootAcc $ Just jVal
                          Just val → throwError "Programmatic error!"
                      processArrayValue acc' = compose (parseNext state' <<< ArrayAcc acc') <<< snoc
                      processPropValue acc' props jVal name = parseNext state' $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing
                      processValue mStr acc' jVal =
                        case acc' of
                          RootAcc mVal → processRootValue mVal jVal
                          ArrayAcc acc'' values → processArrayValue acc'' values jVal
                          ObjectAcc acc'' props mName →
                            maybe
                              (maybe (throwError "Programmatic error!") (parseNext state' <<< ObjectAcc acc'' props <<< Just) mStr)
                              (processPropValue acc'' props jVal)
                              mName
                          _ → throwError "Programmatic error!"
                  in
                  case event of
                    ENumber num → processValue Nothing acc $ fromNumber num
                    ENull → processValue Nothing acc jsonNull
                    EBool bool → processValue Nothing acc $ fromBoolean bool
                    EStringStart _ →
                      case acc of
                        StringAcc _ _ → throwError "Programmatic error!"
                        _ → parseNext state' $ StringAcc acc ""
                    EString _ str →
                      case acc of
                        StringAcc acc' str' → parseNext state' <<< StringAcc acc' $ str' <> str
                        _ → throwError "Programmatic error!"
                    EStringEnd _ →
                      case acc of
                        StringAcc acc' str → processValue (Just str) acc' $ fromString str
                        _ → throwError "Programmatic error!"
                    EArrayStart → parseNext state' $ ArrayAcc acc []
                    EArrayEnd →
                      case acc of
                        ArrayAcc acc' elems → processValue Nothing acc' $ fromArray elems
                        _ → throwError "Programmatic error!"
                    EObjectStart → parseNext state' $ ObjectAcc acc [] Nothing
                    EObjectEnd →
                      case acc of
                        ObjectAcc acc' props mName →
                          case mName of
                            Nothing → processValue Nothing acc' <<< fromObject $ fromFoldable props
                            Just name → throwError "Programmatic error!"
                        _ → throwError "Programmatic error!"
                    EJsonEnd →
                      if isEmbedded
                      then throwError "Programmatic error!"
                      else
                        case acc of
                          RootAcc mVal →
                            case mVal of
                              Just val → pure val
                              _ → throwError "Programmatic error!"
                          _ → throwError "Programmatic error!"
                  )
                  result
        in
        parseNextJsonValueT state >>= meh
  in
  case runExceptT <<< parseNext (stateString emptyStartState jsonStr) $ RootAcc Nothing of
    Identity jsonVal → jsonVal

parseJson ∷ String → Either String Json
parseJson = _parseJson false

parseEmbeddedJson ∷ String → Either String Json
parseEmbeddedJson = _parseJson true
