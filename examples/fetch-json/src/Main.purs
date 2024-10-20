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
  , ParseState
  , initParseState
  , runParseT
  )
import Control.Jsinc.Decoder
  ( class Accumulator
  , class DecodeJsonStream
  , class EndJsonDecode
  , DecodeException(ImplementationError, ParseError)
  , decodeContT
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State.Trans (class MonadState, StateT, get)
import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Promise as Promise
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Either (Either(Left, Right), either)
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

data PathState
  = PathRoot
  | PathObject PathState
  | PathName PathState Boolean String
  | PathArray PathState Int

derive instance eqPathState ∷ Eq PathState
derive instance ordPathState ∷ Ord PathState
derive instance genericPathState ∷ Generic PathState _

instance showPathState ∷ Show PathState where
  show PathRoot = "PathRoot"
  show (PathObject pathState) = "PathObject (" <> show pathState <> ")"
  show (PathName pathState b str) = "PathName (" <> show pathState <> ") " <> show b <> " " <> show str
  show (PathArray pathState ndx) = "PathArray (" <> show pathState <> ") " <> show ndx

instance Applicative m ⇒ Accumulator m PathState where
  initAcc = pure PathRoot

err2 = throwError ImplementationError

showPath2 ∷ ∀ m. Applicative m ⇒ MonadThrow DecodeException m ⇒ PathState → String → m String
showPath2 pathState str =
  case pathState of
  PathRoot → pure str
  PathName pathState' true name →
    case pathState' of
    PathObject pathState'' → showPath2 pathState'' $ "/" <> show name <> str
    _ → err2
  PathArray pathState' ndx → showPath2 pathState' $ "/" <> show ndx <> str
  _ → err2

instance (MonadThrow DecodeException m, MonadEffect m) ⇒ EndJsonDecode Unit PathState m where
  endJsonDecodeT pathState event =
    case event of
    ENumber num → do
      showPath2 pathState (": " <> (maybe (show num) show $ fromNumber num)) >>= log
      case pathState of
        PathRoot → pure $ Right unit
        PathName pathState' true name →
          case pathState' of
          PathObject _ → pure $ Left pathState'
          _ → err2
        PathArray _ _ → pure $ Left pathState
        _ → err2
    _ → err2

instance (MonadThrow DecodeException m, MonadEffect m) ⇒ DecodeJsonStream Unit PathState m where
  decodeJsonT pathState event =
    let logWithPath' str = showPath2 pathState (": " <> str) >>= log
        logWithPath str = do
          logWithPath' str
          case pathState of
            PathRoot → pure $ Right unit
            PathName pathState' true name →
              case pathState' of
              PathObject _ → pure $ Left pathState'
              _ → err2
            PathArray pathState' ndx → pure $ Left (PathArray pathState' $ ndx + 1)
            _ → err2
        upLevel =
          let passName pathState' =
                case pathState' of
                PathName pathState'' _ _ → pure $ Left pathState''
                PathArray pathState'' ndx → pure $ Left (PathArray pathState'' $ ndx + 1)
                _ → err2
          in
          case pathState of
          PathArray pathState' _ → passName pathState'
          PathObject pathState' → passName pathState'
          _ → err2
    in
    case event of
    ENumber num → logWithPath <<< maybe (show num) show $ fromNumber num
    ENull → logWithPath "null"
    EBool bool → logWithPath $ show bool
    EStringStart →
      case pathState of
      PathObject _ → pure $ Left (PathName pathState false "")
      _ → logWithPath' "" *> pure (Left pathState)
    EString str →
      case pathState of
      PathName pathState' false str' → pure $ Left (PathName pathState' false $ str' <> str)
      _ → log (show str) *> pure (Left pathState)
    EStringEnd →
      case pathState of
      PathName pathState' isComplete name → do
        if isComplete
          then
            case pathState' of
            PathObject _ → pure $ Left pathState'
            _ → err2
          else pure $ Left (PathName pathState' true name)
      PathRoot → pure $ Right unit
      PathArray pathState' ndx → pure $ Left (PathArray pathState' $ ndx + 1)
      _ → err2
    EArrayStart → pure $ Left (PathArray pathState 0)
    EArrayEnd →
      case pathState of
      PathArray pathState' _ → upLevel
      _ → err2
    EObjectStart → pure $ Left (PathObject pathState)
    EObjectEnd →
      case pathState of
      PathObject pathState → upLevel
      _ → err2
    _ → err2

decodeJsonResponseStream ∷ ∀ m.
  MonadAff m ⇒
  Response →
  m Unit
decodeJsonResponseStream response = do
  reader ← liftEffect $ body response >>= getReader
  decoder ← liftEffect $ new utf8
  liftAff (Promise.toAffE (unsafeCoerce $ read reader)) >>=
    maybe (log "No data in response") (
        \ chunk → do
          jsonStr ← liftEffect $ decodeWithOptions chunk {stream: true} decoder
          log jsonStr
          (decodeContT ∷
            String →
            (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either PathState Unit) → DecodeException → m Unit) →
            (Tuple (Tuple ParseState (SourcePosition (InPlaceSource String) LineColumnPosition)) (Either PathState Unit) → (Maybe String → m Unit) → m Unit) →
            (Unit → m Unit) →
            m Unit) jsonStr (\ st e → do
              log $ show e
              log $ show st
            )
            (\ _ f → do
              liftAff (Promise.toAffE (unsafeCoerce $ read reader)) >>= maybe
                (f Nothing)
                (\ chunk' → liftEffect (decodeWithOptions chunk' {stream: true} decoder) >>= f <<< Just)
            )
            (\ result → pure unit)
      )

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
      decodeJsonResponseStream response
      log $ "response.status: " <> show (status response)
