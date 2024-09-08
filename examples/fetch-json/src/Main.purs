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
  , ParseException(EOF)
  , ParseState
  , initParseState
  , runParseT
  )
import Control.Jsinc.Decoder
  ( class DecodeJsonStream
  , DecodeExcption(DecodeError, ParseError, ShouldNeverHappen)
  , decodeJsonStreamT
  , decodeLastChunkT
  , decodeLastChunkHelperT
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State.Trans (class MonadState, StateT, get)
import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Show.Generic (genericShow)
import Data.Source
  ( InPlaceSource(InPlaceSource)
  , LineColumnPosition
  , SourcePosition(SourcePosition)
  , initStringPosition
  )
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response (Response, status, body)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (el, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, td, th, tr)
import Web.DOM (Element)
import Web.DOM.Class.NodeOp (appendChild)
import Web.Encoding.TextDecoder (new, decodeWithOptions)
import Web.Encoding.UtfLabel (utf8)
import Web.HTML.HTMLTableCellElement (HTMLTableCellElement)
import Web.Streams.ReadableStream (getReader)
import Web.Streams.Reader (read)

data Prop
  = Actor
  | Repo
  | Payload
  | Author
  | Org
  | Commits

derive instance eqProp ∷ Eq Prop
derive instance ordProp ∷ Ord Prop
derive instance genericProp ∷ Generic Prop _

instance showProp ∷ Show Prop where
  show = genericShow

data RecsState
  = Root
  | Projects
  | Project
  | Proper Prop Boolean
  | Commit
  | Streeng RecsState String
  | Index RecsState Int
  -- | IgnoreValue

derive instance eqRecsState ∷ Eq RecsState
derive instance ordRecsState ∷ Ord RecsState
derive instance genericRecsState ∷ Generic RecsState _

instance showRecsState ∷ Show RecsState where
  show Root = "Root"
  show Projects = "Projects"
  show Project = "Project"
  show (Proper prop b) = "Proper " <> show prop <> " " <> show b
  show Commit = "Commit"
  show (Streeng recsState string) = "Streeng (" <> show recsState <> ") " <> show string
  show (Index recsState int) = "Index (" <> show recsState <> ") " <> show int

newtype TableRowStreamDecoder = TableRowStreamDecoder (Tuple (HashMap Int HTMLTableCellElement) RecsState)

newStateMVal ∷ ∀ m c. Applicative m ⇒ HashMap Int HTMLTableCellElement → RecsState → c → m (Tuple TableRowStreamDecoder c)
newStateMVal hashMap acc = pure <<< Tuple (TableRowStreamDecoder $ Tuple hashMap acc)

newState ∷ ∀ m a. Applicative m ⇒ HashMap Int HTMLTableCellElement → RecsState → m (Tuple TableRowStreamDecoder (Maybe a))
newState hashMap acc = newStateMVal hashMap acc Nothing

erroneous ∷ ∀ m a e a'. MonadThrow (DecodeExcption e a') m ⇒ e → m a
erroneous st = throwError $ DecodeError st

mkCell ∷ ∀ m a e a'. MonadEffect m ⇒ MonadThrow (DecodeExcption e a') m ⇒ e → HashMap Int HTMLTableCellElement → RecsState → String → m (Tuple TableRowStreamDecoder (Maybe a))
mkCell st hashMap acc str =
  case acc of
  Index acc' ndx → if ndx >= 0
    then do
      tdElem ← td [] [txn str]
      newState (insert ndx tdElem hashMap) acc'
    else newState hashMap acc'
  _ → erroneous st

instance (MonadEffect m, MonadReader Element m) ⇒ DecodeJsonStream Unit TableRowStreamDecoder TableRowStreamDecoder m where
  decodeJsonT st@(TableRowStreamDecoder (Tuple hashMap acc)) event =
    let newAcc acc' = newState hashMap acc'
        ahhh acc' str = mkCell st hashMap acc' str
        nameStart = newAcc $ Streeng acc ""
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
      Proper prop false →
        case prop of
        Actor → nameStart
        Repo → nameStart
        Payload → nameStart
        Author → nameStart
        Org → nameStart
        Commits → err
      Commit → nameStart
      Index _ _ → nameStart
      _ → err
    EString _ str →
      case acc of
      Streeng acc' str' → newAcc <<< Streeng acc' $ str' <> str
      _ → err
    EStringEnd _ →
      case acc of
      Streeng acc' str →
        let indexAcc = newAcc <<< Index acc' in
        case acc' of
        Project →
          case str of
          "id" → indexAcc 0
          "type" → indexAcc 1
          "public" → indexAcc 15
          "created_at" → indexAcc 16
          "actor" → newAcc $ Proper Actor true
          "repo" → newAcc $ Proper Repo true
          "payload" → newAcc $ Proper Payload true
          "org" → newAcc $ Proper Org true
          _ → (log $ "unknown name: " <> str) *> (indexAcc (-1))
        Proper prop false →
          case prop of
          Actor →
            case str of
            "id" → indexAcc 2
            "login" → indexAcc 3
            "gravatar_id" → indexAcc 4
            "url" → indexAcc 5
            "avatar_url" → indexAcc 6
            _ → (log $ "unknown name: Actor/" <> str) *> (indexAcc (-1))
          Repo →
            case str of
            "id" → indexAcc 7
            "name" → indexAcc 8
            "url" → indexAcc 9
            _ → (log $ "unknown name: Repo/" <> str) *> (indexAcc (-1))
          Payload →
            case str of
            "ref" → indexAcc 10
            "ref_type" → indexAcc 11
            "master_branch" → indexAcc 12
            "description" → indexAcc 13
            "pusher_type" → indexAcc 14
            "push_id" → indexAcc 17
            "size" → indexAcc 18
            "distinct_size" → indexAcc 19
            "head" → indexAcc 20
            "before" → indexAcc 21
            "commits" → newAcc $ Proper Commits true
            _ → (log $ "unknown name: Payload/" <> str) *> (indexAcc (-1))
          Author →
            case str of
            _ → (log $ "unknown name: Payload/Commit/Author/" <> str) *> (indexAcc (-1))
          Org →
            case str of
            _ → (log $ "unknown name: Org/" <> str) *> (indexAcc (-1))
          Commits → err
        Commit →
          case str of
          "author" → newAcc $ Proper Author true
          _ → (log $ "unknown name: Payload/Commit/" <> str) *> (indexAcc (-1))
        Index _ _ → mkCell st hashMap acc' str
        _ → err
      _ → err
    EArrayStart →
      case acc of
      Root → newAcc Projects
      Proper Commits true → newAcc $ Proper Commits false
      _ → err
    EArrayEnd →
      case acc of
      Projects → newStateMVal hashMap Root $ pure unit
      Proper Commits false → newAcc $ Proper Payload false
      _ → err
    EObjectStart →
      case acc of
      Projects → newAcc Project
      Proper prop true →
        let newAccValue = newAcc $ Proper prop false in
        case prop of
        Actor → newAccValue
        Repo → newAccValue
        Payload → newAccValue
        Author → newAccValue
        Org → newAccValue
        Commits → err
      Proper Commits false → newAcc Commit
      _ → err
    EObjectEnd →
      case acc of
      Project → do
        trElem ← tr [] []
        let buildRow ndx = when (ndx < 17) do
              trElem +< [maybe (td [] [txn "-"]) pure (lookup ndx hashMap) # ndM] # void
              buildRow $ ndx + 1
        buildRow 0
        tbody ← ask
        tbody +< [trElem # nd] # void
        newState empty Projects
      Proper prop false →
        case prop of
        Actor → newAcc Project
        Repo → newAcc Project
        Payload → newAcc Project
        Org → newAcc Project
        Author → newAcc Commit
        Commits → err
      Commit → newAcc $ Proper Commits false
      _ → err
    EJsonEnd → err

  endJsonDecodeT st@(TableRowStreamDecoder (Tuple hashMap acc)) event = do
    let err = erroneous st
    log "Closing things out..."
    case event of
      ENumber num → mkCell st hashMap acc <<< maybe (show num) show $ fromNumber num
      EJsonEnd →
        case acc of
        Root → newStateMVal empty Root $ Just Nothing
        _ → err
      _ → err

whuh ∷ ∀ s c a p s'. s' → Tuple (Tuple a (SourcePosition s p)) c → Tuple (Tuple a (SourcePosition (InPlaceSource s') p)) c
whuh jsonStr (Tuple (Tuple parseState (SourcePosition _ position)) acc) = Tuple (Tuple parseState (SourcePosition (InPlaceSource jsonStr 0) position)) acc

letsBegin ∷ Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) TableRowStreamDecoder
letsBegin = Tuple (Tuple initParseState (initStringPosition "")) (TableRowStreamDecoder $ Tuple empty Root)

decodeJsonResponseStream ∷ ∀ a c e m.
  DecodeJsonStream a e c (ExceptT (DecodeExcption e a) (StateT (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) c) m)) ⇒
  MonadEffect m ⇒
  MonadReader Element m ⇒
  MonadAff m ⇒
  Response →
  c →
  m (Tuple (Tuple (Maybe a) (Maybe (DecodeExcption e a))) (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) c))
decodeJsonResponseStream response startAcc = do
  reader ← liftEffect $ body response >>= getReader
  decoder ← liftEffect $ new utf8
  let stream chunkEndF eofF mA aF state = do
        log "Getting next chunk..."
        mChunk ← liftAff (Promise.toAffE (unsafeCoerce $ read reader))
        maybe (do
            log "No next chunk..."
            chunk ∷ ArrayView Uint8 ← liftEffect $ AB.empty 0
            -- empty out any remaining encoded data in the decoder's buffer
            jsonStr ← liftEffect $ decodeWithOptions chunk {stream: false} decoder
            chunkEndF jsonStr
          )
          (\ chunk → do
            log <<< show $ AB.length chunk
            jsonStr ← liftEffect $ decodeWithOptions chunk {stream: true} decoder
            Tuple result state' ← runParseT decodeJsonStreamT $ whuh jsonStr state
            either
              (\ e →
                case e of
                ParseError EOF → eofF state'
                _ → pure $ Tuple (Tuple mA $ Just e) state'
              )
              (\ a → aF a state')
              result
          )
          mChunk
      checkRestOfStream a state = stream
        (\ jsonStr → do
            Tuple (Tuple mA e) state' ← decodeLastChunkHelperT a $ whuh jsonStr state
            pure $ Tuple (Tuple mA
                case e of
                ParseError EOF → Nothing
                _ → Just e
              ) state'
        )
        (checkRestOfStream a)
        (Just a)
        (\ a' → pure <<< Tuple (Tuple (Just a) <<< Just <<< ShouldNeverHappen $ Just a'))
        state
      decodeNextChunk state = stream
        (\ jsonStr → decodeLastChunkT Nothing $ whuh jsonStr state)
        decodeNextChunk
        Nothing
        checkRestOfStream
        state
  decodeNextChunk $ Tuple (Tuple initParseState (initStringPosition "")) startAcc

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
    launchAff_ do
      request ← liftEffect $ Request.new "test.json"
        { method: GET
        , duplex: Half
        }
      response ← Promise.toAffE $ unsafeCoerce $ Fetch.fetch request
      -- reader ← liftEffect $ body response >>= getReader
      -- decoder ← liftEffect $ new utf8
      -- let stream = do
      --       log "Getting next chunk..."
      --       mChunk ← liftAff (Promise.toAffE (unsafeCoerce $ read reader))
      --       maybe (do
      --           log "No next chunk..."
      --           chunk ∷ ArrayView Uint8 ← liftEffect $ AB.empty 0
      --           -- empty out any remaining encoded data in the decoder's buffer
      --           jsonStr ← liftEffect $ decodeWithOptions chunk {stream: false} decoder
      --           log jsonStr
      --         )
      --         (\ chunk → do
      --           log <<< show $ AB.length chunk
      --           jsonStr ← liftEffect $ decodeWithOptions chunk {stream: true} decoder
      --           log jsonStr
      --           stream
      --         )
      --         mChunk
      -- stream
      Tuple (Tuple mA mE) (Tuple state _) ← runReaderT (decodeJsonResponseStream response (TableRowStreamDecoder $ Tuple empty Root)) tableBody
      maybe (maybe (log "No error, but no indication of success either...") pure mA) (\ e → do
          log $ show state
          case e of
            ParseError err → log $ show err
            ShouldNeverHappen mA' → log $ "Something happened that shouldn't have: " <> show mA <> " and " <> show mA'
            DecodeError (TableRowStreamDecoder (Tuple _ recsState)) → log $ show recsState
        ) mE
      log $ "response.status: " <> show (status response)
