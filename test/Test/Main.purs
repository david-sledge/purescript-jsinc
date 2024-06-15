module Test.Main
  ( main
  )
  where

import Prelude

import Control.Fix (fix)
import Control.Json.Parser (Event(..), ParseException(..), parseJsonMoreDataT, parseJsonNextValueT, parseJsonT)
import Data.Char (fromCharCode)
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assertEqual, assertTrue)
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)

logShow str a = log $ str <> ": " <> show a

main :: Effect Unit
main = do
  log "fix"
  assertEqual
    { actual: (fix (\ fib n ->
        if n < 3
        then 1
        else fib(n - 1) + fib(n - 2)) 7
      )
    , expected: 13
    }

  Tuple result (Tuple parseState _) <- parseJsonT " nul"
  assertEqual
    { actual: result
    , expected: Left EOF
    }

  Tuple result state <- parseJsonMoreDataT parseState "l "
  assertEqual
    { actual: result
    , expected: Right ENull
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Left Done
    }

  Tuple result _ <- parseJsonT "\tfalse \n"
  assertEqual
    { actual: result
    , expected: Right (EBool false)
    }

  Tuple result _ <- parseJsonT "\ttrue"
  assertEqual
    { actual: result
    , expected: Right (EBool true)
    }

  Tuple result state <- parseJsonT "\"\\ud800\\udc00\""
  assertEqual
    { actual: result
    , expected: Right EStringStart
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EString "\xd800\xdc00")
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EStringEnd
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Left Done
    }

  Tuple result state <- parseJsonT "\"rtr\\u0035\\nue\""
  assertEqual
    { actual: result
    , expected: Right EStringStart
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EString "rtr5\nue")
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EStringEnd
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Left Done
    }

  Tuple result state <- parseJsonT "\"rtr\\u00"
  assertEqual
    { actual: result
    , expected: Right EStringStart
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EString "rtr")
    }

  Tuple result (Tuple parseState _) <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Left EOF
    }

  Tuple result state <- parseJsonMoreDataT parseState "35ue\" "
  assertEqual
    { actual: result
    , expected: Right (EString "5ue")
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EStringEnd
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Left Done
    }

  Tuple result _ <- parseJsonT "0 "
  assertEqual
    { actual: result
    , expected: Right (ENumber 0.0)
    }

  Tuple result _ <- parseJsonT "0.0 "
  assertEqual
    { actual: result
    , expected: Right (ENumber (-0.0e+0))
    }

  Tuple result _ <- parseJsonT "-0.0 "
  assertEqual
    { actual: result
    , expected: Right (ENumber 0.0e-3)
    }

  Tuple result _ <- parseJsonT "-0.0e+0 "
  assertEqual
    { actual: result
    , expected: Right (ENumber (-0.0))
    }

  Tuple result _ <- parseJsonT "0.0e-03 "
  assertEqual
    { actual: result
    , expected: Right (ENumber 0.0)
    }

  Tuple result _ <- parseJsonT "0.125e+0022 "
  assertEqual
    { actual: result
    , expected: Right (ENumber 0.125e22)
    }

  Tuple result _ <- parseJsonT "-0.125e-0022 "
  assertEqual
    { actual: result
    , expected: Right (ENumber (-0.125e-22))
    }

  Tuple result state <- parseJsonT "[]"
  assertEqual
    { actual: result
    , expected: Right EArrayStart
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EArrayEnd
    }

  Tuple result state <- parseJsonT "[ null ]"
  assertEqual
    { actual: result
    , expected: Right EArrayStart
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right ENull
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EArrayEnd
    }

  Tuple result state <- parseJsonT "[ true, null ]"

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EBool true)
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right ENull
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EArrayEnd
    }

  Tuple result state <- parseJsonT "\r{ \t}"
  assertEqual
    { actual: result
    , expected: Right EObjectStart
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EObjectEnd
    }

  Tuple result state <- parseJsonT "\r{\"test\": null \t}"
  assertEqual
    { actual: result
    , expected: Right EObjectStart
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EStringStart)
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EString "test")
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right (EStringEnd)
    }

  Tuple result state <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right ENull
    }

  Tuple result _ <- parseJsonNextValueT state
  assertEqual
    { actual: result
    , expected: Right EObjectEnd
    }
