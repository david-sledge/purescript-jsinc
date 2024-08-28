module Data.Json.Parser
  ( Accumulator
  , endParseIncrT
  , parseJson
  , parseJsonIncrT
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
  , endParseT
  , initParseState
  , parseJsonT
  , parseNextJsonValueT
  , startState
  , stateString
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.State (StateT, get, put, runStateT)
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
  , LineColumnPosition(LineColumnPosition)
  , SourcePosition(SourcePosition)
  )
import Data.Tuple (Tuple(Tuple))
import Foreign.Object (fromFoldable)

data Accumulator
  = RootAcc (Maybe Json)
  | StringAcc Accumulator String
  | ArrayAcc Accumulator (Array Json)
  | ObjectAcc Accumulator (Array (Tuple String Json)) (Maybe String)

class FromJsonStream a where
  fromJsonStream ∷ ∀ m s b. Monad m ⇒ Source s Char (MaybeT (ExceptT ParseException (StateT (Tuple (Tuple ParseState s) b) m))) ⇒ Tuple (Tuple ParseState s) b → m (Tuple (Either ParseException a) (Tuple (Tuple ParseState s) b))

-- hmmm result = do
--   either throwError (\ event ->
--       let processRootValue mVal jVal =
--             case mVal of
--               Nothing → put (Tuple state' <<< RootAcc $ Just jVal) *> process
--               Just val → thro $ FlogTheDeveloper parseState
--           processArrayValue acc' values jVal = put (Tuple state' <<< ArrayAcc acc' $ snoc values jVal) *> process
--           processPropValue acc' props jVal name = put (Tuple state' $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing) *> process
--           processValue mStr acc' jVal =
--             case acc' of
--               RootAcc mVal → processRootValue mVal jVal
--               ArrayAcc acc'' values → processArrayValue acc'' values jVal
--               ObjectAcc acc'' props mName →
--                 maybe
--                   (maybe (thro $ FlogTheDeveloper parseState) (\ str → put (Tuple state' <<< ObjectAcc acc'' props $ Just str) *> process) mStr)
--                   (processPropValue acc'' props jVal)
--                   mName
--               _ → thro $ FlogTheDeveloper parseState
--       in
--       case event of
--         ENumber num → processValue Nothing acc $ fromNumber num
--         ENull → processValue Nothing acc jsonNull
--         EBool bool → processValue Nothing acc $ fromBoolean bool
--         EStringStart _ →
--           case acc of
--             StringAcc _ _ → thro $ FlogTheDeveloper parseState
--             _ → put (Tuple state' $ StringAcc acc "") *> parse
--         EString _ str →
--           case acc of
--             StringAcc acc' str' → put (Tuple state' <<< StringAcc acc' $ str' <> str) *> parse
--             _ → thro $ FlogTheDeveloper parseState
--         EStringEnd _ →
--           case acc of
--             StringAcc acc' str → processValue (Just str) acc' $ fromString str
--             _ → thro $ FlogTheDeveloper parseState
--         EArrayStart → put (Tuple state' $ ArrayAcc acc []) *> parse
--         EArrayEnd →
--           case acc of
--             ArrayAcc acc' elems → processValue Nothing acc' $ fromArray elems
--             _ → thro $ FlogTheDeveloper parseState
--         EObjectStart → put (Tuple state' $ ObjectAcc acc [] Nothing) *> parse
--         EObjectEnd →
--           case acc of
--             ObjectAcc acc' props mName →
--               case mName of
--                 Nothing → processValue Nothing acc' <<< fromObject $ fromFoldable props
--                 Just name → thro $ FlogTheDeveloper parseState
--             _ → thro $ FlogTheDeveloper parseState
--         EJsonEnd → thro $ FlogTheDeveloper parseState
--     ) result

_processParseT f h i g = 
  runStateT $ i
    let process = do
          Tuple state acc ← get
          Tuple result state'@(Tuple parseState _) ← f state
          let thro e = h e
          either thro (\ event →
              let processRootValue mVal jVal =
                    case mVal of
                      Nothing → put (Tuple state' <<< RootAcc $ Just jVal) *> process
                      Just val → thro $ FlogTheDeveloper parseState
                  processArrayValue acc' values jVal = put (Tuple state' <<< ArrayAcc acc' $ snoc values jVal) *> process
                  processPropValue acc' props jVal name = put (Tuple state' $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing) *> process
                  processValue mStr acc' jVal =
                    case acc' of
                      RootAcc mVal → processRootValue mVal jVal
                      ArrayAcc acc'' values → processArrayValue acc'' values jVal
                      ObjectAcc acc'' props mName →
                        maybe
                          (maybe (thro $ FlogTheDeveloper parseState) (\ str → put (Tuple state' <<< ObjectAcc acc'' props $ Just str) *> process) mStr)
                          (processPropValue acc'' props jVal)
                          mName
                      _ → thro $ FlogTheDeveloper parseState
              in
              g event acc thro state' processRootValue processArrayValue processPropValue processValue process
            )
            result
    in
    process

parseJsonIncrT ∷ ∀ m s. Monad m ⇒ Source s Char (MaybeT (StateT (Tuple (Tuple ParseState s) Accumulator) m)) ⇒ s → m (Tuple ParseException (Tuple (Tuple ParseState s) Accumulator))
parseJsonIncrT srcState = _processParseT parseNextJsonValueT pure identity (\ event acc thro state'@(Tuple parseState _) processRootValue processArrayValue processPropValue processValue parse ->
      case event of
        ENumber num → processValue Nothing acc $ fromNumber num
        ENull → processValue Nothing acc jsonNull
        EBool bool → processValue Nothing acc $ fromBoolean bool
        EStringStart _ →
          case acc of
            StringAcc _ _ → thro $ FlogTheDeveloper parseState
            _ → put (Tuple state' $ StringAcc acc "") *> parse
        EString _ str →
          case acc of
            StringAcc acc' str' → put (Tuple state' <<< StringAcc acc' $ str' <> str) *> parse
            _ → thro $ FlogTheDeveloper parseState
        EStringEnd _ →
          case acc of
            StringAcc acc' str → processValue (Just str) acc' $ fromString str
            _ → thro $ FlogTheDeveloper parseState
        EArrayStart → put (Tuple state' $ ArrayAcc acc []) *> parse
        EArrayEnd →
          case acc of
            ArrayAcc acc' elems → processValue Nothing acc' $ fromArray elems
            _ → thro $ FlogTheDeveloper parseState
        EObjectStart → put (Tuple state' $ ObjectAcc acc [] Nothing) *> parse
        EObjectEnd →
          case acc of
            ObjectAcc acc' props mName →
              case mName of
                Nothing → processValue Nothing acc' <<< fromObject $ fromFoldable props
                Just name → thro $ FlogTheDeveloper parseState
            _ → thro $ FlogTheDeveloper parseState
        EJsonEnd → thro $ FlogTheDeveloper parseState
    ) (Tuple (Tuple initParseState srcState) $ RootAcc Nothing)

endParseIncrT ∷ ∀ m s. Monad m ⇒ Source s Char (MaybeT (ExceptT ParseException (StateT (Tuple (Tuple ParseState s) Accumulator) m))) ⇒ Tuple (Tuple ParseState s) Accumulator → m (Tuple (Either ParseException Json) (Tuple (Tuple ParseState s) Accumulator))
endParseIncrT = _processParseT endParseT throwError runExceptT (\ event acc thro (Tuple parseState _) _ _ _ processValue _ ->
      case event of
        ENumber num → processValue Nothing acc $ fromNumber num
        EJsonEnd →
          case acc of
            RootAcc mVal →
              case mVal of
                Just val → pure val
                _ → thro $ FlogTheDeveloper parseState
            _ → thro $ FlogTheDeveloper parseState
        _ → thro $ FlogTheDeveloper parseState
    )

parseJsonT :: forall m s. Monad m => Source s Char (MaybeT (StateT (Tuple (Tuple ParseState s) Accumulator) m)) => Source s Char (MaybeT (ExceptT ParseException (StateT (Tuple (Tuple ParseState s) Accumulator) m))) => MonadThrow ParseException m => s -> m (Tuple (Either ParseException Json) (Tuple (Tuple ParseState s) Accumulator))
parseJsonT srcState = do
  Tuple parseException accState <- parseJsonIncrT srcState
  case parseException of
    EOF -> endParseIncrT accState
    _ -> throwError parseException

parseJson ∷ String → Either String Json
parseJson jsonStr = -- do
  -- Tuple parseException accState@(Tuple state'@(Tuple _ (SourcePosition _ (LineColumnPosition pos _ line col))) Accumulator) <- parseJsonIncrT $ initSourceState jsonStr
  -- case parseException of
  --   EOF -> endParseIncrT accState'
  --   _ -> throwError parseException
  let parseNext state acc =
        let meh (Tuple result state'@(Tuple _ (SourcePosition _ (LineColumnPosition pos _ line col)))) =
              either (\ e →
                  case e of
                    EOF → endParseT state' >>= meh
                    FlogTheDeveloper _ → throwError "Whoa! Hol' up!" 
                    DataAfterJson → throwError $ "SyntaxError: Unexpected non-whitespace character after JSON at position " <> show pos <> " (line " <> show (line + 1) <> " column " <> show (col + 1) <> ")"
                    _ → throwError $ show e
                ) (\ event →
                  let processRootValue mVal jVal =
                        case mVal of
                          Nothing → parseNext state' <<< RootAcc $ Just jVal
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
  case runExceptT <<< parseNext (startState jsonStr) $ RootAcc Nothing of
    Identity mJsonVal → mJsonVal
