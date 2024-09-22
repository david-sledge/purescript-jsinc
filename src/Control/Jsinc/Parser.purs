module Control.Jsinc.Parser
  ( CharRead(..)
  , Event(..)
  , Lit(..)
  , NumberRead(..)
  , ParseException(..)
  , ParseState
  , SourceState(..)
  , caseParseState
  , endJsonStreamParseT
  , initParseState
  , parseJsonStreamT
  , runParseT
  , startState
  )
  where

import Prelude

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError )
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Nope (liftMaybe, nope, runNopeT, yup)
import Control.Monad.State (class MonadState, StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (snoc)
import Data.Array as A
import Data.Char (fromCharCode)
import Data.CodePoint.Unicode (decDigitToInt, hexDigitToInt, isControl, isSpace)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Number (pow)
import Data.Show.Generic (genericShow)
import Data.Source
  ( class Source
  , peekSource
  , headSource
  , initialSource
  )
import Data.String.CodeUnits (charAt)
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)
import Data.Tuple (Tuple(Tuple), fst, snd)

data Lit
  = LTrue
  | LFalse
  | LNull

derive instance eqLit ∷ Eq Lit
derive instance ordLit ∷ Ord Lit
derive instance genericLit ∷ Generic Lit _

instance showLit ∷ Show Lit where
  show = genericShow

data CharRead
  = CRClean
  | CREscape
  | CRUnicode Int Int

derive instance eqCharRead ∷ Eq CharRead
derive instance ordCharRead ∷ Ord CharRead
derive instance genericCharRead ∷ Generic CharRead _

instance showCharRead ∷ Show CharRead where
  show = genericShow

data NumberRead
  = NRNegSign
  | NRWholeNum Boolean Number
  | NRDecPoint Boolean Number
  | NRFrac Boolean Number (Number → Number)
  | NRExpInd Number
  | NRExpSign Number Boolean
  | NRExp Number Boolean Number

instance eqNumberRead ∷ Eq NumberRead where
  eq NRNegSign NRNegSign = true
  eq (NRWholeNum b1 num1) (NRWholeNum b2 num2) = eq b1 b2 && eq num1 num2
  eq (NRDecPoint b1 num1) (NRDecPoint b2 num2) = eq b1 b2 && eq num1 num2
  eq (NRFrac b1 num1 f1) (NRFrac b2 num2 f2) = eq b1 b2 && eq num1 num2 && eq (f1 0.0) (f2 0.0)
  eq (NRExpInd num1) (NRExpInd num2) = eq num1 num2
  eq (NRExpSign num1 b1) (NRExpSign num2 b2) = eq num1 num2 && eq b1 b2
  eq (NRExp num1 b1 exp1) (NRExp num2 b2 exp2) = eq num1 num2 && eq b1 b2 && eq exp1 exp2
  eq _ _ = false

instance showNumberRead ∷ Show NumberRead where
  show NRNegSign = "NRNegSign"
  show (NRWholeNum b num) = "NRWholeNum " <> show b <> " " <> show num
  show (NRDecPoint b num) = "NRDecPoint " <> show b <> " " <> show num
  show (NRFrac b num f) = "NRFrac " <> show b <> " " <> show num <> " " <> show (f 0.0 * 10.0)
  show (NRExpInd num) = "NRExpInd " <> show num
  show (NRExpSign num b) = "NRExpSign " <> show num <> " " <> show b
  show (NRExp num b exp) = "NRExp " <> show num <> " " <> show b <> " " <> show exp

data ParseState
  = PRoot
  | PArrayStart ParseState
  | PObjectStart ParseState
  | PArray ParseState
  | PObject ParseState
  | PLiteral Lit Int ParseState
  | PString Boolean CharRead ParseState
  | PNumber NumberRead ParseState
  | PPostName ParseState
  | PPostNameTerm ParseState
  | PPostValue ParseState

derive instance eqParseState ∷ Eq ParseState

showPStateHelper ∷ ParseState → String
showPStateHelper parseState =
  let str = show parseState in
  case parseState of
  PRoot → str
  _ → "(" <> str <> ")"

instance showParseState ∷ Show ParseState where
  show PRoot = "PRoot"
  show (PArrayStart parseState) = "PArrayStart " <> showPStateHelper parseState
  show (PObjectStart parseState) = "PObjectStart " <> showPStateHelper parseState
  show (PArray parseState) = "PArray " <> showPStateHelper parseState
  show (PObject parseState) = "PObject " <> showPStateHelper parseState
  show (PLiteral lit int parseState) = "PLiteral " <> show lit <> " " <> show int <> " " <> showPStateHelper parseState
  show (PString b charRead parseState) = "PString " <> show b <> " " <> show charRead <> " " <> showPStateHelper parseState
  show (PNumber numberRead parseState) = "PNumber " <> show numberRead <> " " <> show parseState
  show (PPostName parseState) = "PPostName " <> showPStateHelper parseState
  show (PPostNameTerm parseState) = "PPostNameTerm " <> showPStateHelper parseState
  show (PPostValue parseState) = "PPostValue " <> showPStateHelper parseState

initParseState ∷ ParseState
initParseState = PRoot

caseParseState ∷ ∀ a. a → (ParseState → a) → (ParseState → a) → (ParseState → a) → (ParseState → a) → (Lit → Int → ParseState → a) → (Boolean → CharRead → ParseState → a) → (NumberRead → ParseState → a) → (ParseState → a) → (ParseState → a) → (ParseState → a) → ParseState → a
caseParseState rootF arrStartF objStartF arrF objF litF strF numF postNameF postNameTermF postValF parseState =
  case parseState of
  PRoot → rootF
  PArrayStart parentState → arrStartF parentState
  PObjectStart parentState → objStartF parentState
  PArray parentState → arrF parentState
  PObject parentState → objF parentState
  PLiteral lit int parentState → litF lit int parentState
  PString b charRead parentState → strF b charRead parentState
  PNumber numberRead parentState → numF numberRead parentState
  PPostName parentState → postNameF parentState
  PPostNameTerm parentState → postNameTermF parentState
  PPostValue parentState → postValF parentState

data ParseException
  = CharExpected Char
  | UnescapedControl
  | InvalidEscape
  | IncompleteUnicodeEscape
  | InvalidValue
  | InvalidValueArrayEnd
  | InvalidDelimArrayEnd
  | InvalidPropName
  | InvalidPropNameObjectEnd
  | InvalidDelimObjectEnd
  | MissingNameTerminator
  | MissingValue
  | IncompleteEscape
  | IncompleteLiteral
  | IncompleteWholeNumber
  | IncompleteExponent
  | IncompleteString
  | MissingPropName
  | UnclosedArray
  | UnclosedObject
  | DataAfterJson
  | FlogTheDeveloper ParseState

derive instance eqParseException ∷ Eq ParseException
derive instance genericParseException ∷ Generic ParseException _

instance showParseException ∷ Show ParseException where
  show = genericShow

data SourceState = SourceState String Int

derive instance eqSourceState ∷ Eq SourceState
derive instance ordSourceState ∷ Ord SourceState
derive instance genericSourceState ∷ Generic SourceState _

instance showSourceState ∷ Show SourceState where
  show = genericShow

data Event
  = ENumber Number
  | ENull
  | EBool Boolean
  | EStringStart Boolean
  | EString Boolean String
  | EStringEnd Boolean
  | EArrayStart
  | EArrayEnd
  | EObjectStart
  | EObjectEnd
  | EJsonEnd

derive instance eqEvent ∷ Eq Event
derive instance ordEvent ∷ Ord Event
derive instance genericEvent ∷ Generic Event _

instance showEvent ∷ Show Event where
  show x = genericShow x

getSourceState ∷ ∀ m a b. MonadState (Tuple b a) m ⇒ m a
getSourceState = snd <$> get

getParseState ∷ ∀ m a b. MonadState (Tuple a b) m ⇒ m a
getParseState = fst <$> get

putParseState ∷ ∀ a b m. MonadState (Tuple a b) m ⇒ a → m Unit
putParseState parseState = modify (\ (Tuple _ srcState) → Tuple parseState srcState) # void

stateTransitionFromEndValue ∷ ∀ a m. MonadState (Tuple ParseState a) m ⇒ MonadThrow ParseException m ⇒ ParseState → m Unit
stateTransitionFromEndValue parentState =
  let next = putParseState $ PPostValue parentState in
  case parentState of
  PRoot → next
  PArray _ → next
  PObject _ → next
  _ → throwError $ FlogTheDeveloper parentState

numberEnd ∷ ∀ m a. MonadState (Tuple ParseState a) m ⇒ MonadThrow ParseException m ⇒ Number → ParseState → m Event
numberEnd num parentState = ENumber num <$ stateTransitionFromEndValue parentState

applySign ∷ ∀ a. Ring a ⇒ Boolean → a → a
applySign isPos = if isPos then identity else negate

--------------------------------------------------------------------------------
startState ∷ ∀ f s d c. Functor f ⇒ Source s d c f ⇒ f (Tuple ParseState s)
startState = Tuple initParseState <$> initialSource

peek ∷ ∀ m a s d c. Monad m ⇒ Source s d c m ⇒ MaybeT (ExceptT ParseException (StateT (Tuple a s) m)) c
peek = getSourceState >>= lift <<< lift <<< lift <<< peekSource >>= liftMaybe

anyChar ∷ ∀ m a s. Monad m ⇒ Source s String Char m ⇒ MaybeT (ExceptT ParseException (StateT (Tuple a s) m)) Char
anyChar = getSourceState >>= lift <<< lift <<< lift <<< headSource >>= maybe nope (\ (Tuple c s') → c <$ modify \ (Tuple parseState _) → Tuple parseState s')

char ∷ ∀ m a s. Monad m ⇒ Source s String Char m ⇒ Char → MaybeT (ExceptT ParseException (StateT (Tuple a s) m)) Char
char c = do
  c' ← peek
  if c == c'
  then anyChar
  else throwError $ CharExpected c

runParseT :: forall s m e a. MaybeT (ExceptT e (StateT s m)) a → s → m (Tuple (Either e (Maybe a)) s)
runParseT = runStateT <<< runExceptT <<< runNopeT

parseJsonStreamT ∷ ∀ m s. Monad m ⇒ Source s String Char m ⇒ Tuple ParseState s → m (Tuple (Either ParseException (Maybe Event)) (Tuple ParseState s))
parseJsonStreamT =
  runParseT
    let parse = do
          let whiteSpace p =
                let recurse = do
                      c ← peek
                      if isSpace $ codePointFromChar c
                      then anyChar *> recurse
                      else p
                in
                recurse
              literalParse lit litPos parentState =
                let recurse litPos' =
                      let litStr = case lit of
                            LTrue → "true"
                            LFalse → "false"
                            LNull → "null"
                      in
                      maybe (do
                          ( case lit of
                            LTrue → EBool true
                            LFalse → EBool false
                            LNull → ENull
                          )
                            <$ stateTransitionFromEndValue parentState
                        ) (\ c →
                          char c *> putParseState (PLiteral lit (litPos' + 1) parentState) *> recurse (litPos' + 1)
                        )
                        $ charAt litPos' litStr
                in
                recurse litPos
              literalStart lit = do
                parseState ← getParseState
                putParseState (PLiteral lit 0 parseState) *> literalParse lit 0 parseState
              stringStart isName = EStringStart isName <$ anyChar <* modify \ (Tuple parseState srcState) → Tuple (PString isName CRClean parseState) srcState
              decDigitCharToInt = decDigitToInt <<< codePointFromChar
          parseState ← getParseState
          case parseState of
            PRoot → whiteSpace do
              c ← peek
              case c of
                'n' → literalStart LNull
                't' → literalStart LTrue
                'f' → literalStart LFalse
                '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parseState)
                '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parseState)
                '"' → stringStart false
                '-' → anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
                _ → maybe
                    (throwError InvalidValue)
                    (\ digit →
                      anyChar
                        *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                        *> parse
                    )
                    $ decDigitCharToInt c
            PArrayStart parentState → whiteSpace do
              c ← peek
              let arrStartToArr p = putParseState (PArray parentState) *> p
              case c of
                'n' → arrStartToArr $ literalStart LNull
                't' → arrStartToArr $ literalStart LTrue
                'f' → arrStartToArr $ literalStart LFalse
                '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart $ PArray parentState)
                '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart $ PArray parentState)
                '"' → arrStartToArr $ stringStart false
                ']' → EArrayEnd <$ anyChar <* stateTransitionFromEndValue parentState
                '-' → arrStartToArr $ anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
                _ → maybe
                    (throwError InvalidValue)
                    (\ digit →
                      arrStartToArr
                        $ anyChar
                        *> putParseState (PNumber (NRWholeNum true $ toNumber digit) $ PArray parentState)
                        *> parse
                    )
                    $ decDigitCharToInt c
            PObjectStart parentState → whiteSpace do
              c ← peek
              let objStartToArr p = putParseState (PObject parentState) *> p
              case c of
                '"' → objStartToArr $ stringStart true
                '}' → EObjectEnd <$ anyChar <* stateTransitionFromEndValue parentState
                _ → throwError InvalidPropNameObjectEnd
            PArray _ → whiteSpace do
              c ← peek
              case c of
                'n' → literalStart LNull
                't' → literalStart LTrue
                'f' → literalStart LFalse
                '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parseState)
                '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parseState)
                '"' → stringStart false
                '-' → anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
                _ → maybe
                    (throwError InvalidValue)
                    (\ digit →
                      anyChar
                        *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                        *> parse
                    )
                    $ decDigitCharToInt c
            PObject _ → whiteSpace do
              c ← peek
              case c of
                '"' → stringStart true
                _ → throwError InvalidPropName
            PString isName charRead parentState →
              let recurse charRead' cpArr =
                    catchError
                      ( yup
                        ( let nextCharRead cRead cpArr' = anyChar *> putParseState (PString isName cRead parentState) *> recurse cRead cpArr'
                              peekChar p = do
                                c ← peek
                                p c $ codePointFromChar c
                          in
                          case charRead' of
                          CRClean → peekChar \ c cp →
                            case c of
                            '\\' → nextCharRead CREscape cpArr
                            '"' → if A.length cpArr > 0
                              then pure <<< EString isName $ fromCodePointArray cpArr
                              else EStringEnd isName <$ anyChar <* putParseState ((if isName then PPostName else PPostValue) parentState)
                            _ → if isControl cp
                              then throwError UnescapedControl
                              else snoc cpArr cp <$ anyChar >>= recurse charRead'
                          CREscape → peekChar \ c _ →
                            let esc c' = nextCharRead CRClean $ snoc cpArr (codePointFromChar c') in
                            case c of
                            '"' → esc '"'
                            '\\' → esc '\\'
                            '/' → esc '/'
                            'b' → esc '\x0008'
                            'f' → esc '\x000c'
                            'n' → esc '\n'
                            'r' → esc '\r'
                            't' → esc '\t'
                            'u' → nextCharRead (CRUnicode 0 0) cpArr
                            _ → throwError InvalidEscape
                          CRUnicode charCount value →
                            if charCount == 4
                            then putParseState (PString isName CRClean parentState) *> recurse CRClean (snoc cpArr <<< codePointFromChar <<< fromMaybe '\xfffd' $ fromCharCode value)
                            else peekChar \ _ cp → maybe
                              (throwError IncompleteUnicodeEscape)
                              (\ digit → nextCharRead (CRUnicode (charCount + 1) $ value * 16 + digit) cpArr)
                              $ hexDigitToInt cp
                      )
                      (
                        if A.length cpArr > 0
                        then pure <<< EString isName $ fromCodePointArray cpArr
                        else nope
                      )
                    )
                    (\ e →
                      if A.length cpArr > 0
                      then pure <<< EString isName $ fromCodePointArray cpArr
                      else throwError e
                    )
              in
              recurse charRead []
            PLiteral lit litPos parentState → literalParse lit litPos parentState
            PPostName parentState → whiteSpace $ char ':' *> putParseState (PPostNameTerm parentState) *> parse
            PPostNameTerm parentState → do
              whiteSpace do
                c ← peek
                case c of
                  'n' → putParseState parentState *> literalStart LNull
                  't' → putParseState parentState *> literalStart LTrue
                  'f' → putParseState parentState *> literalStart LFalse
                  '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parentState)
                  '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parentState)
                  '"' → putParseState parentState *> stringStart false
                  '-' → anyChar *> putParseState (PNumber NRNegSign parentState) *> parse
                  _ → maybe
                      (throwError InvalidValue)
                      (\ digit →
                        anyChar
                          *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parentState)
                          *> parse
                      )
                      $ decDigitCharToInt c
            PPostValue parentState →
              case parentState of
              PRoot → whiteSpace $ throwError DataAfterJson
              PArray gParentState → whiteSpace do
                c' ← peek
                case c' of
                  ',' → anyChar *> putParseState parentState *> parse
                  ']' → EArrayEnd <$ anyChar <* putParseState gParentState <* stateTransitionFromEndValue gParentState
                  _ → throwError InvalidDelimArrayEnd
              PObject gParentState → whiteSpace do
                c' ← peek
                case c' of
                  ',' → anyChar *> putParseState parentState *> parse
                  '}' → EObjectEnd <$ anyChar <* putParseState gParentState <* stateTransitionFromEndValue gParentState
                  _ → throwError InvalidDelimObjectEnd
              _ → throwError $ FlogTheDeveloper parentState
            PNumber numberRead parentState →
              let numberParse numberRead' = do
                    c ← peek
                    let recurse numberRead'' =
                          anyChar
                            *> putParseState (PNumber numberRead'' parentState)
                            *> numberParse numberRead''
                        expStart isPos num = recurse <<< NRExpInd $ applySign isPos num
                        fracF n1 n2 = (n1 + n2) / 10.0
                        digitMaybe no yes = maybe no (yes <<< toNumber) $ decDigitCharToInt c
                    case numberRead' of
                      NRNegSign →
                        digitMaybe
                          (throwError IncompleteWholeNumber)
                          (recurse <<< NRWholeNum false)
                      NRWholeNum isPos num →
                        let intNum = numberEnd (applySign isPos num) parentState in
                        if num == 0.0
                        then
                          case c of
                          '.' → recurse $ NRDecPoint isPos num
                          'E' → expStart isPos num
                          'e' → expStart isPos num
                          _ → intNum
                        else
                          case c of
                          '.' → recurse $ NRDecPoint isPos num
                          'E' → expStart isPos num
                          'e' → expStart isPos num
                          _ → digitMaybe
                            intNum
                            (recurse <<< NRWholeNum isPos <<< add (num * 10.0))
                      NRDecPoint isPos num →
                        digitMaybe
                          (throwError IncompleteExponent)
                          (recurse <<< NRFrac isPos num <<< fracF)
                      NRFrac isPos num accF →
                        let expStart' = expStart isPos $ num + accF 0.0 in
                        digitMaybe
                          ( case c of
                            'E' → expStart'
                            'e' → expStart'
                            _ → numberEnd (applySign isPos $ num + accF 0.0) parentState
                          )
                          (recurse <<< NRFrac isPos num <<< compose accF <<< fracF)
                      NRExpInd num →
                        let toExpSign = recurse <<< NRExpSign num in
                        case c of
                        '+' → toExpSign true
                        '-' → toExpSign false
                        _ →
                          digitMaybe
                            (throwError IncompleteExponent)
                            (recurse <<< NRExp num true)
                      NRExpSign num isPos →
                        digitMaybe
                          (throwError IncompleteExponent)
                          (recurse <<< NRExp num isPos)
                      NRExp num isPos exp →
                        digitMaybe
                          (numberEnd (num * pow 10.0 (applySign isPos exp)) parentState)
                          (recurse <<< NRExp num isPos <<< add (exp * 10.0))
              in
              numberParse numberRead
      in
      parse

endJsonStreamParseT ∷ ∀ m s. Monad m ⇒ Tuple ParseState s → m (Tuple (Either ParseException Event) (Tuple ParseState s))
endJsonStreamParseT = runStateT $ runExceptT do
  parseState ← getParseState
  case parseState of
    PRoot → throwError MissingValue
    PArrayStart _ → throwError UnclosedArray
    PObjectStart _ → throwError UnclosedObject
    PArray _ → throwError MissingValue
    PObject _ → throwError MissingPropName
    PString _ charRead _ →
      case charRead of
      CRClean → throwError IncompleteString
      CREscape → throwError IncompleteEscape
      CRUnicode _ _ → throwError IncompleteUnicodeEscape
    PLiteral _ _ _ → throwError IncompleteLiteral
    PPostName _ → throwError MissingNameTerminator
    PPostNameTerm _ → throwError MissingValue
    PPostValue parentState →
      case parentState of
      PRoot → pure EJsonEnd
      PArray _ → throwError UnclosedArray
      PObject _ → throwError UnclosedObject
      _ → throwError $ FlogTheDeveloper parentState
    PNumber numberRead parentState →
      case numberRead of
      NRNegSign → throwError IncompleteWholeNumber
      NRWholeNum isPos num → numberEnd (applySign isPos num) parentState
      NRDecPoint _ _ → throwError IncompleteWholeNumber
      NRFrac isPos num accF → numberEnd (applySign isPos $ num + accF 0.0) parentState
      NRExpInd _ → throwError IncompleteExponent
      NRExpSign _ _ → throwError IncompleteExponent
      NRExp num isPos exp → numberEnd (num * pow 10.0 (applySign isPos exp)) parentState
