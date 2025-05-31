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
    )
  )
import Control.Jsinc.Decoder
  ( class DecodeJsonStream
  , class EndJsonDecode
  , DecodeException(DecodeError)
  )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (class MonadReader, class MonadAsk, ask, runReaderT)
import Control.Promise as Promise
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Int (fromNumber)
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Timer (setTimeout)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response (status)
import Promise (new)
import Promise.Aff (toAffE)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (el, eln, nd, ndM, remove, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, td, th, tr)
import Web.DOM (Element)
import Web.DOM.Class.NodeOp (class NodeOp, appendChild, childNodes, firstChild)
import Web.DOM.NodeList (length)
import Web.Fetch.Response.Jsinc (decodeJsonResponseStream)

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

err ∷ forall m t c a. MonadThrow (DecodeException c a) m ⇒ String → m t
err = throwError <<< DecodeError

showPath ∷ ∀ m. Applicative m ⇒ MonadThrow (DecodeException PathState Unit) m ⇒ PathState → String → m String
showPath pathState str =
  case pathState of
  PathRoot → pure str
  PathName pathState' true name →
    case pathState' of
    PathObject pathState'' → showPath pathState'' $ "/" <> show name <> str
    _ → err $ "Programmatic error: parent of PathName should be PathObject: " <> show pathState'
  PathArray pathState' ndx → showPath pathState' $ "/" <> show ndx <> str
  _ → err $ "Programmatic error: pathState should be PathRoot, PathName, or PathArray: " <> show pathState

logToUi ∷ ∀ n m. MonadAff m ⇒ MonadAsk n m ⇒ NodeOp n ⇒ String → m Unit
logToUi str = do
  trow <- tr [] [ td [Tuple "style" "white-space: pre-wrap"] [ txn str ] # ndM ] # ndM
  tableBod <- ask
  appendChild trow tableBod
  len <- childNodes tableBod >>= liftEffect <<< length
  pure unit
  when (len > 1000) do
    mChild <- firstChild tableBod
    maybe (pure unit) (void <<< remove) mChild
  liftAff <<< delay $ Milliseconds 1.0

instance (MonadThrow (DecodeException PathState Unit) m, MonadAff m, MonadReader Element m) ⇒ EndJsonDecode PathState Unit m where
  endJsonDecodeT pathState num = do
    showPath pathState (": " <> (maybe (show num) show $ fromNumber num)) >>= logToUi
    case pathState of
      PathRoot → pure $ Right unit
      PathName pathState' true _ →
        case pathState' of
        PathObject _ → pure $ Left pathState'
        _ → err $ "Programmatic error: parent of PathName should be PathObject: " <> show pathState'
      PathArray _ _ → pure $ Left pathState
      _ → err $ "Programmatic error: pathState should be PathRoot, PathName, or PathArray: " <> show pathState

instance (MonadThrow (DecodeException PathState Unit) m, MonadAff m, MonadReader Element m) ⇒ DecodeJsonStream PathState Unit m where
  decodeJsonT pathState event =
    let logWithPath' str = showPath pathState (": " <> str) >>= logToUi
        logWithPath str = do
          logWithPath' str
          case pathState of
            PathRoot → pure $ Right unit
            PathName pathState' true _ →
              case pathState' of
              PathObject _ → pure $ Left pathState'
              _ → err $ "Programmatic error: parent of PathName should be PathObject: " <> show pathState'
            PathArray pathState' ndx → pure $ Left (PathArray pathState' $ ndx + 1)
            _ → err $ "Programmatic error: pathState should be PathRoot, PathName, or PathArray: " <> show pathState
        upLevel =
          let passName pathState' =
                case pathState' of
                PathName pathState'' _ _ → pure $ Left pathState''
                PathArray pathState'' ndx → pure $ Left (PathArray pathState'' $ ndx + 1)
                _ → err $ "Programmatic error: pathState should be PathName, or PathArray: " <> show pathState'
          in
          case pathState of
          PathArray pathState' _ → passName pathState'
          PathObject pathState' → passName pathState'
          _ → err $ "Programmatic error: pathState should be PathObject, or PathArray: " <> show pathState
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
      _ → logToUi ("\t" <> show str) *> pure (Left pathState)
    EStringEnd →
      case pathState of
      PathName pathState' isComplete name → do
        if isComplete
          then
            case pathState' of
            PathObject _ → pure $ Left pathState'
            _ → err $ "Programmatic error: pathState should be PathObject: " <> show pathState'
          else pure $ Left (PathName pathState' true name)
      PathRoot → pure $ Right unit
      PathArray pathState' ndx → pure $ Left (PathArray pathState' $ ndx + 1)
      _ → err $ "Programmatic error: pathState should be PathName, PathRoot, or PathArray: " <> show pathState
    EArrayStart → pure $ Left (PathArray pathState 0)
    EArrayEnd →
      case pathState of
      PathArray _ _ → upLevel
      _ → err $ "Programmatic error: pathState should be PathArray: " <> show pathState
    EObjectStart → pure $ Left (PathObject pathState)
    EObjectEnd →
      case pathState of
      PathObject _ → upLevel
      _ → err $ "Programmatic error: pathState should be PathObject: " <> show pathState

main ∷ Effect Unit
main = do
  onReady_ \ _ → do
    tableBody ← el "tbody" [] []
    bind docBody <<< appendChild =<< table [] [ nd tableBody ]
    launchAff_ do
      request ← liftEffect $ Request.new "test.json"
        { method: GET
        , duplex: Half
        }
      response ← toAffE $ Fetch.fetch request
      runReaderT (decodeJsonResponseStream PathRoot response) tableBody
      log $ "response.status: " <> show (status response)
