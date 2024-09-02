module Main where

import Prelude

import Control.Jsinc.Parser
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
import Control.Jsinc.Decoder (class DecodeJsonStream, DecodeExcption(DecodeError))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.State.Trans (evalStateT)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (Either(Left), either)
import Data.HTTP.Method (Method(GET))
import Data.HashMap (HashMap, insert)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response as Response
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (el, eln, nd, ndM, txn)
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, td, th, tr)
import Web.DOM.Class.NodeOp (appendChild)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement)
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

data RecsState
  = Root
  | Recs
  | Project
  | Actor
  | ActorProp
  | Repo
  | RepoProp
  | Payload
  | PayloadProp
  | Name RecsState String
  | Index RecsState Int

newtype TableRowStreamDecoder = TableRowStreamDecoder (Tuple (HashMap Int HTMLTableCellElement) RecsState)

instance MonadEffect m ⇒ DecodeJsonStream Unit TableRowStreamDecoder TableRowStreamDecoder m where
  decodeJsonT st@(TableRowStreamDecoder (Tuple hashMap acc)) event =
    let newState hashMap' = pure <<< Left <<< TableRowStreamDecoder <<< Tuple hashMap'
        newAcc acc' = newState hashMap acc'
        err = throwError $ DecodeError st
        ahhh acc str =
          case acc of
            Index acc' ndx → do
              tdElem <- td [] [txn str]
              newState (insert ndx tdElem hashMap) acc'
            _ → err
    in
    case event of
      ENumber num →
        ahhh acc <<< maybe (show num) show $ fromNumber num
      ENull → ahhh acc "-"
      EBool bool → ahhh acc $ show bool
      EStringStart _ →
        case acc of
          Project → newAcc $ Name acc ""
          Actor → newAcc $ Name acc ""
          Repo → newAcc $ Name acc ""
          Payload → newAcc $ Name acc ""
          Index _ _ → newAcc $ Name acc ""
          _ → err
      EString _ str →
        case acc of
          Name acc' str' → newAcc <<< Name acc $ str' <> str
          _ → err
      EStringEnd _ →
        case acc of
          Name acc' str →
            let indexAcc = newAcc <<< Index acc' in
            case acc' of
              Project →
                case str of
                  "id" → indexAcc 0
                  "type" → indexAcc 1
                  "public" → indexAcc 15
                  "created_at" → indexAcc 16
                  "actor" → newAcc ActorProp
                  "repo" → newAcc RepoProp
                  "payload" → newAcc PayloadProp
                  _ → (log $ "unknown name: " <> str) *> (indexAcc (-1))
              Actor →
                case str of
                  "id" → indexAcc 2
                  "login" → indexAcc 3
                  "gravatar_id" → indexAcc 4
                  "url" → indexAcc 5
                  "avatar_url" → indexAcc 6
                  _ → (log $ "unknown name: " <> str) *> (indexAcc (-1))
              Repo →
                case str of
                  "id" → indexAcc 7
                  "name" → indexAcc 8
                  "url" → indexAcc 9
                  _ → (log $ "unknown name: " <> str) *> (indexAcc (-1))
              Payload →
                case str of
                  "ref" → indexAcc 10
                  "ref_type" → indexAcc 11
                  "master_branch" → indexAcc 12
                  "description" → indexAcc 13
                  "pusher_type" → indexAcc 14
                  _ → (log $ "unknown name: " <> str) *> (indexAcc (-1))
              Index _ _ → newAcc $ Name acc ""
              _ → err
          _ → err
      EArrayStart →
        case acc of
          Root → newAcc Recs
          _ → err
      EArrayEnd →
        case acc of
          Recs → newAcc Root
          _ → err
      EObjectStart →
        case acc of
          Recs → newAcc Project
          ActorProp → newAcc Actor
          RepoProp → newAcc Repo
          PayloadProp → newAcc Payload
          _ → err
      EObjectEnd →
        case acc of
          Project → do
            -- fill out table row
            newAcc Recs
          Actor → newAcc Project
          Repo → newAcc Project
          Payload → newAcc Project
          _ → err
      EJsonEnd → err

  endJsonDecodeT acc event = throwError $ DecodeError acc
    -- let f = pure <<< Left
    --     g = pure <<< pure
    --     err = throwError $ DecodeError acc
    -- in
    -- case event of
    --   ENumber num →
    --     let jVal = fromNumber num in
    --     case acc of
    --       Root → g $ Just jVal
    --       ArrayAcc acc' values → f <<< ArrayAcc acc' $ snoc values jVal
    --       ObjectAcc acc' props mName →
    --         maybe
    --           err
    --           (\ name → f $ ObjectAcc acc' (snoc props $ Tuple name jVal) Nothing)
    --           mName
    --       _ → err
    --   EJsonEnd →
    --     case acc of
    --       Root → g Nothing
    --       _ → err
    --   _ → err

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
