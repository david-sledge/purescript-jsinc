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
  , ParseState
  , emptyStartState
  , endJsonStreamParseT
  , initParseState
  , parseJsonStreamT
  , runParseT
  , stateString)
import Control.Jsinc.Decoder (class DecodeJsonStream, DecodeExcption(DecodeError), decodeJsonStreamT)
import Control.Monad (class Monad)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Reader (class MonadReader, ask)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (Either(Left), either)
import Data.HTTP.Method (Method(GET))
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Source (SourcePosition, InPlaceSource, LineColumnPosition, initStringPosition)
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
import Web.Chain.DOM (el, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, td, th, tr)
import Web.DOM (Element)
import Web.DOM.Class.NodeOp (appendChild)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.HTML.HTMLTableRowElement (HTMLTableRowElement)
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement)
import Web.Streams.ReadableStream (getReader)
import Web.Streams.Reader (read)

hLog str a = log $ str <> show a

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

newStateMVal hashMap acc = pure <<< Tuple (TableRowStreamDecoder $ Tuple hashMap acc)

newState hashMap acc = newStateMVal hashMap acc Nothing

erroneous st = throwError $ DecodeError st

mkCell st hashMap acc str =
  case acc of
  Index acc' ndx → do
    tdElem <- td [] [txn str]
    newState (insert ndx tdElem hashMap) acc'
  _ → erroneous st

instance (MonadEffect m, MonadReader Element m) ⇒ DecodeJsonStream Unit TableRowStreamDecoder TableRowStreamDecoder m where
  decodeJsonT st@(TableRowStreamDecoder (Tuple hashMap acc)) event =
    let newAcc acc' = newState hashMap acc'
        ahhh acc' str = mkCell st hashMap acc' str
        nameStart = newAcc $ Name acc ""
        err = erroneous st
    in
    case event of
    ENumber num →
      ahhh acc <<< maybe (show num) show $ fromNumber num
    ENull → ahhh acc "-"
    EBool bool → ahhh acc $ show bool
    EStringStart _ →
      case acc of
      Project → nameStart
      Actor → nameStart
      Repo → nameStart
      Payload → nameStart
      Index _ _ → nameStart
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
        Index _ _ → nameStart
        _ → err
      _ → err
    EArrayStart →
      case acc of
      Root → newAcc Recs
      _ → err
    EArrayEnd →
      case acc of
      Recs → newStateMVal hashMap Root $ pure unit
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
        trElem <- tr [] []
        let buildRow ndx = do
              trElem +< [maybe (td [] [txn "-"]) pure (lookup ndx hashMap) # ndM] # void
              when (ndx < 17) <<< buildRow $ ndx + 1
        buildRow 0
        tbody <- ask
        tbody +< [trElem # nd] # void
        newState empty Recs
      Actor → newAcc Project
      Repo → newAcc Project
      Payload → newAcc Project
      _ → err
    EJsonEnd → err

  endJsonDecodeT st@(TableRowStreamDecoder (Tuple hashMap acc)) event =
    let err = erroneous st in
    case event of
    ENumber num → mkCell st hashMap acc <<< maybe (show num) show $ fromNumber num
    EJsonEnd →
      case acc of
      Root → newStateMVal hashMap Root Nothing
      _ → err
    _ → err

hmmm :: forall a m. MonadReader Element m => MonadEffect m => DecodeJsonStream Unit TableRowStreamDecoder TableRowStreamDecoder m => String -> m (Tuple (Tuple (Maybe Unit) (DecodeExcption TableRowStreamDecoder)) (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) TableRowStreamDecoder))
hmmm jsonStr = do
  Tuple result state ← runParseT decodeJsonStreamT <<< Tuple (Tuple initParseState $ initStringPosition jsonStr) <<< TableRowStreamDecoder $ Tuple empty Root
  either (\ e -> pure $ Tuple (Tuple Nothing e) state)
    (\ value → do
      Tuple (result' ∷ Either (DecodeExcption TableRowStreamDecoder) Unit) state'@(Tuple _ acc) ← runParseT decodeJsonStreamT state
      either (\ e -> pure $ Tuple (Tuple (pure value) e) state') (const <<< pure $ Tuple (Tuple Nothing $ DecodeError acc) state') result'
    ) result

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
