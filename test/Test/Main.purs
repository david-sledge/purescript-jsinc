module Test.Main
  ( main
  )
  where

import Prelude

import Control.Applicative (unless)
import Control.Jsinc.Parser (Event(..), ParseException(..), ParseState, endJsonStreamParseT, initParseState, parseJsonStreamT)
import Data.Argonaut
  ( Json
  , fromArray
  , fromBoolean
  , fromNumber
  , fromObject
  , fromString
  , jsonNull
  , toNumber
  )
import Data.Argonaut.Decode.Parser as A
import Data.Array (uncons)
import Data.Char (fromCharCode)
import Data.Either (Either(..), either)
import Data.Jsinc.Argonaut (parseJson)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Source (class Source, InPlaceSource(InPlaceSource), LineColumnPosition(LineColumnPosition), SourcePosition(SourcePosition), advance, peekSource, headSource, refillSource)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assert, assertEqual, assertTrue)
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)

main ∷ Effect Unit
main = do
  let source = InPlaceSource "null" 0

  argh ← peekSource source
  assertEqual
    { actual: argh
    , expected: Just 'n'
    }

  argh ← headSource source
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ InPlaceSource "null" 1)
    }

  argh' ← refillSource "   " source
  assertEqual
    { actual: argh'
    , expected: InPlaceSource "null   " 0
    }

  maybe (pure unit) (\ (Tuple c srcPos) → do
      argh ← headSource srcPos
      assertEqual
        { actual: argh
        , expected: Just (Tuple 'u' $ InPlaceSource "null" 2)
        }
    ) argh

  let pos = LineColumnPosition 0 false 0 0

  argh ← advance 'n' pos
  assertEqual
    { actual: argh
    , expected: LineColumnPosition 1 false 0 1
    }

  let srcPos = SourcePosition source pos
      state = Tuple initParseState srcPos

  argh ← peekSource srcPos
  assertEqual
    { actual: argh
    , expected: Just 'n'
    }

  argh ← headSource srcPos
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ SourcePosition (InPlaceSource "null" 1) (LineColumnPosition 1 false 0 1))
    }

  maybe (pure unit) (\ (Tuple c srcPos) → do
      argh ← headSource srcPos
      assertEqual
        { actual: argh
        , expected: Just (Tuple 'u' $ SourcePosition (InPlaceSource "null" 2) (LineColumnPosition 2 false 0 2))
        }
    ) argh

  argh ← headSource srcPos
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ SourcePosition (InPlaceSource "null" 1) (LineColumnPosition 1 false 0 1))
    }

  Tuple result state ← parseJsonStreamT state
  assertEqual
    { actual: result
    , expected: Right $ Just ENull
    }

  let wrap str posi = SourcePosition (InPlaceSource str 0) posi
      wrap' str = wrap str pos

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' " nul")
    [ Tuple parseJsonStreamT <<< Tuple (Right Nothing) $ refillSource "l "
    , Tuple parseJsonStreamT $ Tuple (Right $ Just ENull) pure
    , Tuple parseJsonStreamT $ Tuple (Right Nothing) pure
    ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\tfalse \n") [Right $ Just (EBool false)]
  log "false test"
  compareToArgonaut "\tfalse \n"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\ttrue") [Right $ Just (EBool true)]
  log "true test"
  compareToArgonaut "\ttrue"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\"\\ud800\\udc00\"")
    [ Right $ Just EStringStart
    , Right <<< Just $ EString "\xd800\xdc00"
    , Right $ Just EStringEnd
    , Right Nothing
    ]
  compareToArgonaut "\"\\ud800\\udc00\""

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\"rtr\\u0035\\nue\"")
    [ Right $ Just EStringStart
    , Right <<< Just $ EString "rtr5\nue"
    , Right $ Just EStringEnd
    , Right Nothing
    ]
  compareToArgonaut "\"rtr\\u0035\\nue\""

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' "\"rtr\\u00")
    [ Tuple parseJsonStreamT $ Tuple (Right $ Just EStringStart) pure
    , Tuple parseJsonStreamT $ Tuple (Right <<< Just $ EString "rtr") pure
    , Tuple parseJsonStreamT $ Tuple (Right Nothing) $ refillSource "35ue\" "
    , Tuple parseJsonStreamT $ Tuple (Right <<< Just $ EString "5ue") pure
    , Tuple parseJsonStreamT $ Tuple (Right $ Just EStringEnd) pure
    , Tuple parseJsonStreamT $ Tuple (Right Nothing) pure
    ]

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' "")
    [ Tuple parseJsonStreamT <<< Tuple (Right Nothing) $ refillSource ""
    , Tuple parseJsonStreamT $ Tuple (Right Nothing) pure
    ]

  ------------------------------------------------------------------------------
  Tuple result _ ← parseJsonStreamT <<< Tuple initParseState $ wrap' "0 "
  assertEqual
    { actual: result
    , expected: Right $ Just (ENumber 0.0)
    }
  compareToArgonaut "0 "

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.0 ")
    [ Right $ Just (ENumber (-0.0e+0)) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.0 ")
    [ Right $ Just (ENumber 0.0e-3) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.0e+0 ")
    [ Right $ Just (ENumber (-0.0)) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.0e-03 ")
    [ Right $ Just (ENumber 0.0) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.125e+0022 ")
    [ Right $ Just (ENumber 0.125e22) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.125e-0022 ")
    [ Right $ Just (ENumber (-0.125e-22)) ]

  ------------------------------------------------------------------------------
  runTest2 (Tuple initParseState $ wrap' "-0.125e-0022")
    [ Tuple parseJsonStreamT $ Right Nothing
    , Tuple (\ state → do
        Tuple result state' ← endJsonStreamParseT state
        pure $ Tuple ((map ENumber) <$> result) state') <<< Right <<< Just $ ENumber (-0.125e-22)
    ]
  compareToArgonaut "-0.125e-0022"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[]")
    [ Right $ Just EArrayStart
    , Right $ Just EArrayEnd
    ]
  compareToArgonaut "[]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[ null ]")
    [ Right $ Just EArrayStart
    , Right $ Just ENull
    , Right $ Just EArrayEnd
    ]
  compareToArgonaut "[ null ]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[ true, null ]")
    [ Right $ Just EArrayStart
    , Right <<< Just $ EBool true
    , Right $ Just ENull
    , Right $ Just EArrayEnd
    ]
  compareToArgonaut "[ true, null ]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\r{ \t}")
    [ Right $ Just EObjectStart
    , Right $ Just EObjectEnd
    ]
  compareToArgonaut "\r{ \t}"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\r{\"test\": null \t}")
    [ Right $ Just EObjectStart
    , Right $ Just EStringStart
    , Right <<< Just $ EString "test"
    , Right $ Just EStringEnd
    , Right $ Just ENull
    , Right $ Just EObjectEnd
    ]
  compareToArgonaut "\r{\"test\": null \t}"

  ------------------------------------------------------------------------------
  compareToArgonaut "\r{\"test\": null \t" -- UnclosedObject
  compareToArgonaut "[5" -- UnclosedArray
  compareToArgonaut "[\"test\"," -- MissingValue
  compareToArgonaut "[\"test\"" -- UnclosedArray
  compareToArgonaut "[\"test" -- IncompleteString
  compareToArgonaut "{\"test\":0," -- MissingPropName
  compareToArgonaut "{\"test\":0" -- UnclosedObject
  compareToArgonaut "{\"test\":" -- MissingValue
  compareToArgonaut "{\"test\"" -- MissingNameTerminator
  compareToArgonaut "{\"test" -- IncompleteString
  compareToArgonaut "[{\"id\":\"2489651045\",\"type\":\"CreateEvent\",\"actor\":{\"id\":665991,\"login\":\"petroav\",\"gravatar_id\":\"\",\"url\":\"https://api.github.com/users/petroav\",\"avatar_url\":\"https://avatars.githubusercontent.com/u/665991?\"},\"repo\":{\"id\":28688495,\"name\":\"petroav/6.828\",\"url\":\"https://api.github.com/repos/petroav/6.828\"},\"payload\":{\"ref\":\"master\",\"ref_type\":\"branch\",\"master_branch\":\"master\",\"description\":\"Solution to homework and assignments from MIT's 6.828 (Operating Systems Engineering). Done in my spare time.\",\"pusher_type\":\"user\"},\"public\":true,\"created_at\":\"2015-01-01T15:00:00Z\"}\n]"
  compareToArgonaut "{} j" -- DataAfterJson

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "{\"test\":")
    [ Right $ Just EObjectStart
    , Right $ Just EStringStart
    , Right <<< Just $ EString "test"
    , Right $ Just EStringEnd
    , Right Nothing
    ]

compareToArgonaut ∷ String → Effect Unit
compareToArgonaut str = do
  let argonautParse = A.parseJson str
      jsincParse = parseJson str
      result = either (\ _ → Nothing) Just argonautParse == either (\ _ → Nothing) Just jsincParse
  -- if result
  -- then
  --   pure $ either (\ _ → trace jsincParse \ _ → unit) (\ _ → unit) jsincParse
  -- else do
    -- trace argonautParse \ _ →
    --   trace jsincParse \ _ →
  unless result $ trace jsincParse \ _ → assert result

runTest ∷ ∀ s. Source s String Char Effect ⇒ Tuple ParseState s → Array (Either ParseException (Maybe Event)) → Effect Unit
runTest state expected =
  case uncons expected of
    Just { head: x, tail: expected' } → do
      Tuple result state' ← parseJsonStreamT state
      assertEqual
        { actual: result
        , expected: x
        }
      runTest state' expected'
    Nothing → --trace state \ _ →
      pure unit

runTest2 ∷ ∀ s. Source s String Char Effect ⇒ Tuple ParseState s → Array (Tuple (Tuple ParseState s → Effect (Tuple (Either ParseException (Maybe Event)) (Tuple ParseState s))) (Either ParseException (Maybe Event))) → Effect Unit
runTest2 state expected =
  case uncons expected of
    Just { head: Tuple f x, tail: expected' } → do
      Tuple result state' ← f state
      assertEqual
        { actual: result
        , expected: x
        }
      runTest2 state' expected'
    Nothing → pure unit

runTest3 ∷ ∀ s. Source s String Char Effect ⇒ Tuple ParseState s → Array (Tuple (Tuple ParseState s → Effect (Tuple (Either ParseException (Maybe Event)) (Tuple ParseState s))) (Tuple (Either ParseException (Maybe Event)) (s → Effect s))) → Effect Unit
runTest3 state expected =
  case uncons expected of
    Just { head: Tuple f (Tuple x g), tail: expected' } → do
      Tuple result (Tuple parseState srcState) ← f state
      assertEqual
        { actual: result
        , expected: x
        }
      srcState' ← g srcState
      runTest3 (Tuple parseState srcState') expected'
    Nothing → pure unit
