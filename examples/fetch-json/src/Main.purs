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
import Control.Promise as Promise
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Int (fromNumber)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Fetch.Core as Fetch
import Fetch.Core.Duplex (Duplex(Half))
import Fetch.Core.Request as Request
import Fetch.Core.Response (status)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (el, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (docBody, table, td, th, tr)
import Web.DOM (Element)
import Web.DOM.Class.NodeOp (appendChild)
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

instance (MonadThrow (DecodeException PathState Unit) m, MonadEffect m) ⇒ EndJsonDecode PathState Unit m where
  endJsonDecodeT pathState num = do
    showPath pathState (": " <> (maybe (show num) show $ fromNumber num)) >>= log
    case pathState of
      PathRoot → pure $ Right unit
      PathName pathState' true _ →
        case pathState' of
        PathObject _ → pure $ Left pathState'
        _ → err $ "Programmatic error: parent of PathName should be PathObject: " <> show pathState'
      PathArray _ _ → pure $ Left pathState
      _ → err $ "Programmatic error: pathState should be PathRoot, PathName, or PathArray: " <> show pathState

instance (MonadThrow (DecodeException PathState Unit) m, MonadEffect m) ⇒ DecodeJsonStream PathState Unit m where
  decodeJsonT pathState event =
    let logWithPath' str = showPath pathState (": " <> str) >>= log
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
      _ → log (show str) *> pure (Left pathState)
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
      decodeJsonResponseStream PathRoot response
      log $ "response.status: " <> show (status response)
