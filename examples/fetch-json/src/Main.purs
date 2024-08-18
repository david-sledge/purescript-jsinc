module Main where

import Prelude

import Control.Fix (fix)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (maybe)
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

hLog str a = log $ show a

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
    let recurse = Promise.toAffE (unsafeCoerce $ read reader)
          >>= maybe (do
                chunk :: ArrayView Uint8 <- liftEffect $ AB.empty 0
                jsonStr <- liftEffect $ decodeWithOptions chunk {stream: false} decoder
                --parseJsonStringT jsonStr
                pure unit
              )
              (\ chunk -> do
                liftEffect <<< hLog "chunkX" $ AB.length chunk
                jsonStr <- liftEffect $ decodeWithOptions chunk {stream: true} decoder
                log jsonStr
                recurse)
    recurse
    liftEffect <<< hLog "response.status" $ Response.status response
