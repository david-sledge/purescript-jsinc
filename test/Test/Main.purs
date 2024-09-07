module Test.Main
  ( main
  )
  where

import Prelude

import Control.Jsinc.Parser (Event(..), ParseException(..), ParseState, endJsonStreamParseT, initParseState, parseJsonStreamT)
import Control.Monad.Nope (NopeT, runNopeT)
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
import Data.Source (class Source, InPlaceSource(InPlaceSource), LineColumnPosition(LineColumnPosition), SourcePosition(SourcePosition), advance, peekSource, headSource)
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assert, assertEqual, assertTrue)
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)

main ∷ Effect Unit
main = do
  let source = InPlaceSource "null" 0

  argh ← runNopeT $ peekSource source
  assertEqual
    { actual: argh
    , expected: Just 'n'
    }

  argh ← runNopeT $ headSource source
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ InPlaceSource "null" 1)
    }

  maybe (pure unit) (\ (Tuple c srcPos) → do
      argh ← runNopeT $ headSource srcPos
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

  argh ← runNopeT $ peekSource srcPos
  assertEqual
    { actual: argh
    , expected: Just 'n'
    }

  argh ← runNopeT $ headSource srcPos
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ SourcePosition (InPlaceSource "null" 1) (LineColumnPosition 1 false 0 1))
    }

  maybe (pure unit) (\ (Tuple c srcPos) → do
      argh ← runNopeT $ headSource srcPos
      assertEqual
        { actual: argh
        , expected: Just (Tuple 'u' $ SourcePosition (InPlaceSource "null" 2) (LineColumnPosition 2 false 0 2))
        }
    ) argh

  argh ← runNopeT $ headSource srcPos
  assertEqual
    { actual: argh
    , expected: Just (Tuple 'n' $ SourcePosition (InPlaceSource "null" 1) (LineColumnPosition 1 false 0 1))
    }

  Tuple result state ← parseJsonStreamT state
  assertEqual
    { actual: result
    , expected: Right ENull
    }

  let wrap str posi = SourcePosition (InPlaceSource str 0) posi
      wrap' str = wrap str pos

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' " nul")
    [ Tuple parseJsonStreamT $ Tuple (Left EOF) \ (SourcePosition _ posi) → wrap "l " posi
    , Tuple parseJsonStreamT $ Tuple (Right ENull) identity
    , Tuple parseJsonStreamT $ Tuple (Left EOF) identity
    ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\tfalse \n") [Right (EBool false)]
  compareToArgonaut "\tfalse \n"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\ttrue") [Right (EBool true)]
  compareToArgonaut "\ttrue"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\"\\ud800\\udc00\"")
    [ Right $ EStringStart false
    , Right (EString false "\xd800\xdc00")
    , Right $ EStringEnd false
    , Left EOF
    ]
  compareToArgonaut "\"\\ud800\\udc00\""

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\"rtr\\u0035\\nue\"")
    [ Right $ EStringStart false
    , Right $ EString false "rtr5\nue"
    , Right $ EStringEnd false
    , Left EOF
    ]
  compareToArgonaut "\"rtr\\u0035\\nue\""

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' "\"rtr\\u00")
    [ Tuple parseJsonStreamT $ Tuple (Right $ EStringStart false) identity
    , Tuple parseJsonStreamT $ Tuple (Right $ EString false "rtr") identity
    , Tuple parseJsonStreamT $ Tuple (Left EOF) \ (SourcePosition _ posi) → wrap "35ue\" " posi
    , Tuple parseJsonStreamT $ Tuple (Right $ EString false "5ue") identity
    , Tuple parseJsonStreamT $ Tuple (Right $ EStringEnd false) identity
    , Tuple parseJsonStreamT $ Tuple (Left EOF) identity
    ]

  ------------------------------------------------------------------------------
  runTest3 (Tuple initParseState $ wrap' "")
    [ Tuple parseJsonStreamT $ Tuple (Left EOF) \ (SourcePosition _ posi) → wrap "" posi
    , Tuple parseJsonStreamT $ Tuple (Left EOF) identity
    ]

  ------------------------------------------------------------------------------
  Tuple result _ ← parseJsonStreamT <<< Tuple initParseState $ wrap' "0 "
  assertEqual
    { actual: result
    , expected: Right (ENumber 0.0)
    }
  compareToArgonaut "0 "

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.0 ")
    [ Right (ENumber (-0.0e+0)) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.0 ")
    [ Right (ENumber 0.0e-3) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.0e+0 ")
    [ Right (ENumber (-0.0)) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.0e-03 ")
    [ Right (ENumber 0.0) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "0.125e+0022 ")
    [ Right (ENumber 0.125e22) ]

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "-0.125e-0022 ")
    [ Right (ENumber (-0.125e-22)) ]

  ------------------------------------------------------------------------------
  runTest2 (Tuple initParseState $ wrap' "-0.125e-0022")
    [ Tuple parseJsonStreamT $ Left EOF
    , Tuple endJsonStreamParseT $ Right (ENumber (-0.125e-22))
    ]
  compareToArgonaut "-0.125e-0022"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[]")
    [ Right EArrayStart
    , Right EArrayEnd
    ]
  compareToArgonaut "[]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[ null ]")
    [ Right EArrayStart
    , Right ENull
    , Right EArrayEnd
    ]
  compareToArgonaut "[ null ]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "[ true, null ]")
    [ Right EArrayStart
    , Right (EBool true)
    , Right ENull
    , Right EArrayEnd
    ]
  compareToArgonaut "[ true, null ]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\r{ \t}")
    [ Right EObjectStart
    , Right EObjectEnd
    ]
  compareToArgonaut "\r{ \t}"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "\r{\"test\": null \t}")
    [ Right EObjectStart
    , Right (EStringStart true)
    , Right (EString true "test")
    , Right (EStringEnd true)
    , Right ENull
    , Right EObjectEnd
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
  compareToArgonaut "{} j" -- DataAfterJson
  compareToArgonaut "[{\"id\":\"2489651045\",\"type\":\"CreateEvent\",\"actor\":{\"id\":665991,\"login\":\"petroav\",\"gravatar_id\":\"\",\"url\":\"https://api.github.com/users/petroav\",\"avatar_url\":\"https://avatars.githubusercontent.com/u/665991?\"},\"repo\":{\"id\":28688495,\"name\":\"petroav/6.828\",\"url\":\"https://api.github.com/repos/petroav/6.828\"},\"payload\":{\"ref\":\"master\",\"ref_type\":\"branch\",\"master_branch\":\"master\",\"description\":\"Solution to homework and assignments from MIT's 6.828 (Operating Systems Engineering). Done in my spare time.\",\"pusher_type\":\"user\"},\"public\":true,\"created_at\":\"2015-01-01T15:00:00Z\"}\n]"

  ------------------------------------------------------------------------------
  runTest (Tuple initParseState $ wrap' "{\"test\"")
    [ Right EObjectStart
    , Right (EStringStart true)
    , Right (EString true "test")
    , Right (EStringEnd true)
    , Left EOF
    ]

compareToArgonaut ∷ String → Effect Unit
compareToArgonaut str = do
  let argonautParse = A.parseJson str
      jsincParse = parseJson str
      result = either (const Nothing) Just argonautParse == either (const Nothing) Just jsincParse
  if result
  then
    pure $ either (const $ trace jsincParse \ _ → unit) (const unit) jsincParse
  else do
    trace argonautParse \ _ →
      trace jsincParse \ _ →
      assert (either (const Nothing) Just (A.parseJson str) == either (const Nothing) Just (parseJson str))

runTest ∷ ∀ s. Source s Char (NopeT Effect) ⇒ Tuple ParseState s → Array (Either ParseException Event) → Effect Unit
runTest state expected =
  case uncons expected of
    Just { head: x, tail: expected' } → do
      Tuple result state' ← parseJsonStreamT state
      assertEqual
        { actual: result
        , expected: x
        }
      runTest state' expected'
    Nothing → trace state \ _ →pure unit

runTest2 ∷ ∀ s. Source s Char (NopeT Effect) ⇒ Tuple ParseState s → Array (Tuple (Tuple ParseState s → Effect (Tuple (Either ParseException Event) (Tuple ParseState s))) (Either ParseException Event)) → Effect Unit
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

runTest3 ∷ ∀ s. Source s Char (NopeT Effect) ⇒ Tuple ParseState s → Array (Tuple (Tuple ParseState s → Effect (Tuple (Either ParseException Event) (Tuple ParseState s))) (Tuple (Either ParseException Event) (s → s))) → Effect Unit
runTest3 state expected =
  case uncons expected of
    Just { head: Tuple f (Tuple x g), tail: expected' } → do
      Tuple result (Tuple parseState srcState) ← f state
      assertEqual
        { actual: result
        , expected: x
        }
      runTest3 (Tuple parseState $ g srcState) expected'
    Nothing → pure unit
