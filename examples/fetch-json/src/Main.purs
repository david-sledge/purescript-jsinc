module Main where

import Prelude

import Control.Json.Parser
  ( parseNextJsonValueT
  , Event(ENumber, ENull, EBool, EStringStart, EString, EStringEnd, EArrayStart, EArrayEnd, EObjectStart, EObjectEnd)
  , ParseException(EOF, Msg, FlogTheDeveloper, Done)
  , emptyStartState
  , endParseT
  , stateString)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (either)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response as Response
import Unsafe.Coerce (unsafeCoerce)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.Streams.ReadableStream (getReader)
import Web.Streams.Reader (read)

hLog str a = log $ str <> show a

main :: Effect Unit
main = do
  decoder <- new utf8
  launchAff_ do
    request <- liftEffect $ Request.new "test.json"
      { method: GET
      , duplex: Half
      }
    response <- Promise.toAffE $ unsafeCoerce $ Fetch.fetch request
    responseBody <- liftEffect $ Response.body response
    reader <- liftEffect $ getReader responseBody
    let recurse state = Promise.toAffE (unsafeCoerce $ read reader)
          >>= maybe (do
                log "No more chunks..."
                chunk :: ArrayView Uint8 <- liftEffect $ AB.empty 0
                jsonStr <- liftEffect $ decodeWithOptions chunk {stream: false} decoder
                let state_ = stateString state jsonStr
                    parseNext stateArg = do
                      Tuple result state'@(Tuple parseState _) <- parseNextJsonValueT stateArg
                      -- hLog "" parseState
                      either (\ e -> do
                          case e of
                            EOF -> do
                              Tuple result' state''@(Tuple parseState' _) <- endParseT state'
                              log "All done!"
                            Msg msg -> log $ "Error: " <> msg
                            FlogTheDeveloper _ -> log "Whoa! Hol' up!"
                            Done -> log "All done!"
                        ) (\ event -> do
                          -- hLog "" event
                          case event of
                            ENumber num -> parseNext state'
                            ENull -> parseNext state'
                            EBool bool -> parseNext state'
                            EStringStart -> parseNext state'
                            EString str -> parseNext state'
                            EStringEnd -> parseNext state'
                            EArrayStart -> parseNext state'
                            EArrayEnd -> parseNext state'
                            EObjectStart -> parseNext state'
                            EObjectEnd -> parseNext state'
                        ) result
                parseNext state_)
              (\ chunk -> do
                -- liftEffect <<< hLog "" $ AB.length chunk
                jsonStr <- liftEffect $ decodeWithOptions chunk {stream: true} decoder
                let state_ = stateString state jsonStr
                    parseNext stateArg = do
                      Tuple result state'@(Tuple parseState _) <- parseNextJsonValueT stateArg
                      -- hLog "" parseState
                      either (\ e -> do
                          case e of
                            EOF -> log "Getting next chunk..." *> recurse state'
                            Msg msg -> log $ "Error: " <> msg
                            FlogTheDeveloper _ -> log "Whoa! Hol' up!" 
                            Done -> log "All done!"
                        ) (\ event -> do
                          -- hLog "" event
                          case event of
                            ENumber num -> parseNext state'
                            ENull -> parseNext state'
                            EBool bool -> parseNext state'
                            EStringStart -> parseNext state'
                            EString str -> parseNext state'
                            EStringEnd -> parseNext state'
                            EArrayStart -> parseNext state'
                            EArrayEnd -> parseNext state'
                            EObjectStart -> parseNext state'
                            EObjectEnd -> parseNext state'
                        ) result
                parseNext state_)
    recurse emptyStartState
    liftEffect <<< hLog "response.status" $ Response.status response
