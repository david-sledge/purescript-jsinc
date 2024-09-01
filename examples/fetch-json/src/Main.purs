module Main where

import Prelude

import Control.Json.Core.Parser
  ( Event
    ( ENumber
    , ENull
    , EBool
    , EStringStart
    , EString
    , EStringEnd
    , EArrayStart
    , EArrayEnd
    , EObjectStart
    , EObjectEnd
    , EJsonEnd
    )
  , ParseException(EOF, FlogTheDeveloper, DataAfterJson)
  , emptyStartState
  , endJsonStreamParseT
  , parseJsonStreamT
  , stateString)
import Control.Monad.State.Trans (evalStateT)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (either)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response as Response
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (el, eln, nd, ndM, txn)
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, th, tr)
import Web.DOM.Class.NodeOp (appendChild)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.Streams.ReadableStream (getReader)
import Web.Streams.Reader (read)

hLog str a = log $ str <> show a

type TRow =
  { id ∷ Maybe String
  , type ∷ Maybe String
  , actor_id ∷ Maybe Int
  , actor_login ∷ Maybe String
  , actor_gravatar_id ∷ Maybe String
  , actor_url ∷ Maybe String
  , actor_avatar_url ∷ Maybe String
  , repo_id ∷ Maybe Int
  , repo_name ∷ Maybe String
  , repo_url ∷ Maybe String
  , payload_ref ∷ Maybe String
  , payload_ref_type ∷ Maybe String
  , payload_master_branch ∷ Maybe String
  , payload_description ∷ Maybe String
  , payload_pusher_type ∷ Maybe String
  , public ∷ Maybe Boolean
  , created_at ∷ Maybe String
  }

main ∷ Effect Unit
main = do
  onReady_ \ _ → do
    tableBody ← el "tbody" [] []
    bind docBody <<< appendChild =<< table []
      [ eln "thead" []
        [ tr []
          [ th [ Tuple "rowspan" "2" ] [ txn "ID" ] # ndM
          , th [ Tuple "rowspan" "2" ] [ txn "Type" ] # ndM
          , th [ Tuple "colspan" "5" ] [ txn "Actor" ] # ndM
          , th [ Tuple "colspan" "3" ] [ txn "Repo" ] # ndM
          , th [ Tuple "colspan" "5" ] [ txn "Payload" ] # ndM
          , th [ Tuple "rowspan" "2" ] [ txn "Is Public?" ] # ndM
          , th [ Tuple "rowspan" "2" ] [ txn "Date Created" ] # ndM
          ] # ndM
        , tr []
          [ th [] [ txn "ID" ] # ndM
          , th [] [ txn "Login" ] # ndM
          , th [] [ txn "Gravatar ID" ] # ndM
          , th [] [ txn "URL" ] # ndM
          , th [] [ txn "Avatar URL" ] # ndM
          , th [] [ txn "ID" ] # ndM
          , th [] [ txn "Name" ] # ndM
          , th [] [ txn "URL" ] # ndM
          , th [] [ txn "Ref" ] # ndM
          , th [] [ txn "Ref Type" ] # ndM
          , th [] [ txn "Master Branch" ] # ndM
          , th [] [ txn "Description" ] # ndM
          , th [] [ txn "Pusher Type" ] # ndM
          ] # ndM
        ]
      , nd tableBody
      ]
    decoder ← new utf8
    launchAff_ do
      request ← liftEffect $ Request.new "test.json"
        { method: GET
        , duplex: Half
        }
      response ← Promise.toAffE $ unsafeCoerce $ Fetch.fetch request
      responseBody ← liftEffect $ Response.body response
      reader ← liftEffect $ getReader responseBody
      let parseNext stateArg eofF = do
            Tuple result state'@(Tuple parseState _) ← parseJsonStreamT stateArg
            -- hLog "" parseState
            either (\ e → do
                case e of
                  EOF → eofF state'
                  FlogTheDeveloper _ → log "Whoa! Hol' up!" 
                  DataAfterJson → log "All done!"
                  _ → log $ show e
              ) (\ event → do
                -- hLog "" event
                case event of
                  ENumber num → parseNext state' eofF
                  ENull → parseNext state' eofF
                  EBool bool → parseNext state' eofF
                  EStringStart isName → parseNext state' eofF
                  EString isName str → parseNext state' eofF
                  EStringEnd isName → parseNext state' eofF
                  EArrayStart → parseNext state' eofF
                  EArrayEnd → parseNext state' eofF
                  EObjectStart → parseNext state' eofF
                  EObjectEnd → parseNext state' eofF
                  EJsonEnd → parseNext state' eofF
              ) result
      let recurse state = liftAff (Promise.toAffE (unsafeCoerce $ read reader))
            >>= maybe (do
                  log "No more chunks..."
                  chunk ∷ ArrayView Uint8 ← liftEffect $ AB.empty 0
                  jsonStr ← liftEffect $ decodeWithOptions chunk {stream: false} decoder
                  parseNext (stateString state jsonStr) \ state' → do
                    Tuple result' state''@(Tuple parseState' _) ← endJsonStreamParseT state'
                    log "All done!")
                (\ chunk → do
                  -- liftEffect <<< hLog "" $ AB.length chunk
                  jsonStr ← liftEffect $ decodeWithOptions chunk {stream: true} decoder
                  parseNext (stateString state jsonStr) \ state' → log "Getting next chunk..." *> recurse state')
      evalStateT (recurse emptyStartState) []
      liftEffect <<< hLog "response.status" $ Response.status response
