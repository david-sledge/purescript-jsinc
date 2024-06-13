module Test.Main where

import Prelude

import Control.Fix (fix)
import Control.Json.Parser (parseJsonT, parseJsonMoreDataT, parseJsonNextValueT)
import Data.Either (either)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class.Console (log)

logShow str a = log $ str <> ": " <> show a

main :: Effect Unit
main = do
  log "test"

  logShow "fix" $ fix (\ fib n ->
    if n < 3
    then 1
    else fib(n - 1) + fib(n - 2)) 7

  stateAndResult@(Tuple result (Tuple parseState _)) <- parseJsonT " nul"
  logShow "main1" stateAndResult
  either (\ e -> parseJsonMoreDataT parseState "l " >>= logShow "main1.1") (const $ log "Should have been an error!") result

  stateAndResult@(Tuple result (Tuple parseState _)) <- parseJsonT "\tfalse \n"
  logShow "main2" stateAndResult

  stateAndResult@(Tuple result (Tuple parseState _)) <- parseJsonT "\rtrue"
  logShow "main3" stateAndResult

  stateAndResult@(Tuple result state) <- parseJsonT "\"rtr\\u0035\\nue\""
  logShow "main6" stateAndResult
  either (logShow "main6.1") (const $ do
      stateAndResult@(Tuple result state) <- parseJsonNextValueT state
      logShow "main6.2.2" stateAndResult
      either (logShow "main6.2.2.1") (const $ parseJsonNextValueT state >>= logShow "main6.2.2.2") result
    ) result

  stateAndResult@(Tuple result state) <- parseJsonT "\"rtr\\u00"
  logShow "main6" stateAndResult
  either (logShow "main6.1") (const $ do
      stateAndResult@(Tuple result state) <- parseJsonNextValueT state
      logShow "main6.2.2" stateAndResult
      either (logShow "main6.2.2.1") (const $ do
        stateAndResult@(Tuple result (Tuple parseState _)) <- parseJsonNextValueT state
        logShow "main6.2.2.2" stateAndResult
        either (\ e -> do
          stateAndResult@(Tuple result state) <- parseJsonMoreDataT parseState "35ue\" "
          logShow "main5.2" stateAndResult
          either (logShow "main5.2.1") (const $ parseJsonNextValueT state >>= logShow "main5.2.2") result
        ) (const $ log "Should have been an error!") result
      ) result
    ) result

  parseJsonT "0" >>= logShow "main4"

  stateAndResult@(Tuple result state) <- parseJsonT "[]"
  logShow "main4" stateAndResult
  either (logShow "main4.1") (const $ parseJsonNextValueT state >>= logShow "main4.2") result

  stateAndResult@(Tuple result state) <- parseJsonT "[ null ]"
  logShow "main5" stateAndResult
  either (logShow "main5.1") (const $ do
      stateAndResult@(Tuple result state) <- parseJsonNextValueT state
      logShow "main5.2" stateAndResult
      either (logShow "main5.2.1") (const $ parseJsonNextValueT state >>= logShow "main5.2.2") result
    ) result

  stateAndResult@(Tuple result state) <- parseJsonT "[ true, null ]"
  logShow "main6" stateAndResult
  either (logShow "main6.1") (const $ do
      stateAndResult@(Tuple result state) <- parseJsonNextValueT state
      logShow "main6.2.2" stateAndResult
      either (logShow "main6.2.2.1") (const $ do
        stateAndResult@(Tuple result state) <- parseJsonNextValueT state
        logShow "main6.2.2.2" stateAndResult
        either (logShow "main6.2.2.2.1") (const $ parseJsonNextValueT state >>= logShow "main6.2.2.2.2") result
      ) result
    ) result

  stateAndResult@(Tuple result state) <- parseJsonT "\r{ \t}"
  logShow "main7" stateAndResult
  either (logShow "main7.1") (const $ parseJsonNextValueT state >>= logShow "main7.2") result

  stateAndResult@(Tuple result state) <- parseJsonT "\r{\"test\": null \t}"
  logShow "main8" stateAndResult
  either (logShow "main8.1") (const $ do
      stateAndResult@(Tuple result state) <- parseJsonNextValueT state
      logShow "main8.2" stateAndResult
      either (logShow "main8.2.1") (const $ do
        stateAndResult@(Tuple result state) <- parseJsonNextValueT state
        logShow "main8.2.2" stateAndResult
        either (logShow "main8.2.2.1") (const $ do
          stateAndResult@(Tuple result state) <- parseJsonNextValueT state
          logShow "main8.2.2.2" stateAndResult
          either (logShow "main8.2.2.2.1") (const $ do
            stateAndResult@(Tuple result state) <- parseJsonNextValueT state
            logShow "main8.2.2.2.2" stateAndResult
            either (logShow "main8.2.2.2.2.1") (const $ parseJsonNextValueT state >>= logShow "main8.2.2.2.2.2") result
          ) result
        ) result
      ) result
    ) result
