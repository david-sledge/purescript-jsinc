module Control.Json.Parser
  ( CharRead(..)
  , Event(..)
  , Lit(..)
  , NumberRead(..)
  , ParseException(..)
  , ParseState
  , SourceState(..)
  , endParseT
  , initParseState
  , parseMoreJsonStringT
  , parseMoreJsonDataT
  , parseJsonNextValueT
  , parseJsonStringT
  , parseJsonT
  )
  where

import Prelude

import Control.Fix (fix)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Nope (NopeT, runNopeT)
import Control.Monad.State (class MonadState, StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (snoc)
import Data.Array as A
import Data.Char (fromCharCode)
import Data.CodePoint.Unicode (decDigitToInt, hexDigitToInt, isControl, isSpace)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, maybe)
import Data.Number (pow)
import Data.Show.Generic (genericShow)
import Data.Source as S
import Data.String (singleton)
import Data.String.CodeUnits (charAt, slice)
import Data.String.CodePoints (codePointFromChar, fromCodePointArray)
import Data.Tuple (Tuple(Tuple), fst, snd)

data ParseException
  = EOF
  | Msg String
  | FlogTheDeveloper
  | Done

derive instance eqParseException ∷ Eq ParseException
derive instance ordParseException ∷ Ord ParseException
derive instance genericParseException ∷ Generic ParseException _

instance showParseException ∷ Show ParseException where
  show = genericShow

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
  | PPostNameTermin ParseState
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
  show (PLiteral lit int parseState) = "PObjectStart " <> show lit <> " " <> show int <> " " <> showPStateHelper parseState
  show (PString b charRead parseState) = "PString " <> show b <> " " <> show charRead <> " " <> showPStateHelper parseState
  show (PNumber numberRead parseState) = "PNumber " <> show numberRead <> " " <> show parseState
  show (PPostName parseState) = "PPostName " <> showPStateHelper parseState
  show (PPostNameTermin parseState) = "PPostNameTermin " <> showPStateHelper parseState
  show (PPostValue parseState) = "PPostValue " <> showPStateHelper parseState

initParseState ∷ ParseState
initParseState = PRoot

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
  | EStringStart
  | EString String
  | EStringEnd
  | EArrayStart
  | EArrayEnd
  | EObjectStart
  | EObjectEnd

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
    _ → throwError FlogTheDeveloper

numberEnd ∷ ∀ m a. Functor m ⇒ MonadState (Tuple ParseState a) m ⇒ MonadThrow ParseException m ⇒ Number → ParseState → m Event
numberEnd num parentState = ENumber num <$ stateTransitionFromEndValue parentState

applySign ∷ ∀ a. Ring a ⇒ Boolean → a → a
applySign isPos = if isPos then identity else negate

--------------------------------------------------------------------------------
peek ∷ ∀ m a s c. Monad m ⇒ S.Source s c (NopeT m) ⇒ ExceptT ParseException (StateT (Tuple a s) m) c
peek = getSourceState >>= lift <<< lift <<< runNopeT <<< S.peekSource >>= maybe (throwError EOF) pure

anyChar ∷ ∀ m a s. Monad m ⇒ S.Source s Char (NopeT m) ⇒ ExceptT ParseException (StateT (Tuple a s) m) Char
anyChar = getSourceState >>= lift <<< lift <<< runNopeT <<< S.headSource >>= maybe (throwError EOF) (\ (Tuple c s') → c <$ modify \ (Tuple parseState _) → Tuple parseState s')

char ∷ ∀ m a s. Monad m ⇒ S.Source s Char (NopeT m) ⇒ Char → ExceptT ParseException (StateT (Tuple a s) m) Char
char c = do
  c' <- peek
  if c == c'
  then anyChar
  else throwError <<< Msg $ "Expected '" <> singleton (codePointFromChar c) <> "', found '" <> singleton (codePointFromChar c') <> "'"

parseJsonNextValueT ∷ ∀ m s. Monad m ⇒ S.Source s Char (NopeT m) ⇒ Tuple ParseState s → m (Tuple (Either ParseException Event) (Tuple ParseState s))
parseJsonNextValueT =
  runStateT (runExceptT $ fix \ parse → do
      let whiteSpace p =
            fix \ recurse → do
              c <- peek
              if isSpace $ codePointFromChar c
              then anyChar *> recurse
              else p
          literalParse lit litPos parentState =
            fix (\ recurse litPos' →
                let litStr = case lit of
                      LTrue → "true"
                      LFalse → "false"
                      LNull → "null"
                in
                maybe (do
                    (case lit of
                      LTrue → EBool true
                      LFalse → EBool false
                      LNull → ENull)
                      <$ stateTransitionFromEndValue parentState
                  ) (\ c →
                    char c *> putParseState (PLiteral lit (litPos' + 1) parentState) *> recurse (litPos' + 1)
                  )
                  $ charAt litPos' litStr
              )
              litPos
          literalStart lit = do
            parseState ← getParseState
            putParseState (PLiteral lit 0 parseState) *> literalParse lit 0 parseState
          stringStart isName = EStringStart <$ anyChar <* modify \ (Tuple parseState srcState) → Tuple (PString isName CRClean parseState) srcState
          decDigitCharToInt = decDigitToInt <<< codePointFromChar
      parseState ← getParseState
      case parseState of
        PRoot → whiteSpace do
          c <- peek
          case c of
            'n' → literalStart LNull
            't' → literalStart LTrue
            'f' → literalStart LFalse
            '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parseState)
            '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parseState)
            '"' → stringStart false
            '-' → anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
            _ → maybe
                (throwError $ Msg "Invalid character. Expecting a value (null, Boolean, string, number, array, or object).")
                (\ digit →
                  anyChar
                    *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                    *> parse
                )
                $ decDigitCharToInt c
        PArrayStart parentState → whiteSpace do
          c <- peek
          let arrStartToArr p = putParseState (PArray parentState) *> p
          case c of
            'n' → arrStartToArr $ literalStart LNull
            't' → arrStartToArr $ literalStart LTrue
            'f' → arrStartToArr $ literalStart LFalse
            '[' → arrStartToArr $ EArrayStart <$ anyChar <* putParseState (PArrayStart parseState)
            '{' → arrStartToArr $ EObjectStart <$ anyChar <* putParseState (PObjectStart parseState)
            '"' → arrStartToArr $ stringStart false
            ']' → EArrayEnd <$ anyChar <* stateTransitionFromEndValue parentState
            '-' → arrStartToArr $ anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
            _ → maybe
                (throwError $ Msg "Invalid character. Expecting the end of the array or a value (null, Boolean, string, number, array, or object).")
                (\ digit →
                  arrStartToArr
                    $ anyChar
                    *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                    *> parse
                )
                $ decDigitCharToInt c
        PObjectStart parentState → whiteSpace do
          c <- peek
          let objStartToArr p = putParseState (PObject parentState) *> p
          case c of
            '"' → objStartToArr $ stringStart true
            '}' → EObjectEnd <$ anyChar <* stateTransitionFromEndValue parentState
            _ → throwError $ Msg "Invalid character. Expecting the end of the object or a string."
        PArray _ → whiteSpace do
          c <- peek
          case c of
            'n' → literalStart LNull
            't' → literalStart LTrue
            'f' → literalStart LFalse
            '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parseState)
            '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parseState)
            '"' → stringStart false
            '-' → anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
            _ → maybe
                (throwError $ Msg "Invalid character. Expecting a value (null, Boolean, string, number, array, or object).")
                (\ digit →
                  anyChar
                    *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                    *> parse
                )
                $ decDigitCharToInt c
        PObject _ → whiteSpace do
          c <- peek
          case c of
            '"' → stringStart true
            _ → throwError $ Msg "Invalid character. Expecting a string."
        PString isName charRead parentState →
          fix ( \ recurse charRead' cpArr →
            catchError
              ( let nextCharRead cRead cpArr' = anyChar *> putParseState (PString isName cRead parentState) *> recurse cRead cpArr'
                    peekChar p = do
                      c <- peek
                      p c $ codePointFromChar c
                in
                case charRead' of
                  CRClean → peekChar \ c cp →
                    case c of
                      '\\' → nextCharRead CREscape cpArr
                      '"' → if A.length cpArr > 0
                        then pure <<< EString $ fromCodePointArray cpArr
                        else EStringEnd <$ anyChar <* putParseState ((if isName then PPostName else PPostValue) parentState)
                      _ → if isControl cp
                        then throwError $ Msg "Unescaped control characters are not allowed."
                        else snoc cpArr cp <$ anyChar >>= recurse charRead'
                  CREscape → peekChar \ c cp →
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
                      _ → throwError <<< Msg $ "Invalid escape sequence: \"\\" <> singleton cp <> "\""
                  CRUnicode charCount value →
                        if charCount == 4
                        then putParseState (PString isName CRClean parentState) *> recurse CRClean (snoc cpArr <<< codePointFromChar <<< fromMaybe '\xfffd' $ fromCharCode value)
                        else peekChar \ _ cp → maybe
                          (throwError <<< Msg $ "Invalid hex digit: \"\\" <> singleton cp <> "\"")
                          (\ digit → nextCharRead (CRUnicode (charCount + 1) $ value * 16 + digit) cpArr)
                          $ hexDigitToInt cp
            )
            (\ e →
              if A.length cpArr > 0
              then pure <<< EString $ fromCodePointArray cpArr
              else throwError e
            )
          ) charRead []
        PLiteral lit litPos parentState → literalParse lit litPos parentState
        PPostName parentState → whiteSpace $ char ':' *> putParseState (PPostNameTermin parentState) *> parse
        PPostNameTermin parentState → do
          putParseState parentState
          whiteSpace do
            c <- peek
            case c of
              'n' → literalStart LNull
              't' → literalStart LTrue
              'f' → literalStart LFalse
              '[' → EArrayStart <$ anyChar <* putParseState (PArrayStart parentState)
              '{' → EObjectStart <$ anyChar <* putParseState (PObjectStart parentState)
              '"' → stringStart false
              '-' → anyChar *> putParseState (PNumber NRNegSign parseState) *> parse
              _ → maybe
                  (throwError $ Msg "Invalid character. Expecting a value (null, Boolean, string, number, array, or object).")
                  (\ digit →
                    anyChar
                      *> putParseState (PNumber (NRWholeNum true $ toNumber digit) parseState)
                      *> parse
                  )
                  $ decDigitCharToInt c
        PPostValue parentState → do
          case parentState of
            PRoot → throwError Done
            PArray gParentState → whiteSpace do
              c' <- peek
              case c' of
                ',' → anyChar *> putParseState parentState *> parse
                ']' → EArrayEnd <$ anyChar <* putParseState gParentState <* stateTransitionFromEndValue gParentState
                _ → throwError $ Msg "Invalid character. Expecting the end of the array or a comma."
            PObject gParentState → whiteSpace do
              c' <- peek
              case c' of
                ',' → anyChar *> putParseState parentState *> parse
                '}' → EObjectEnd <$ anyChar <* putParseState gParentState <* stateTransitionFromEndValue gParentState
                _ → throwError $ Msg "Invalid character. Expecting the end of the object or a comma."
            _ → throwError FlogTheDeveloper
        PNumber numberRead parentState →
          fix (\ numberParse numberRead' → do
            c <- peek
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
                  (throwError $ Msg "")
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
                  (throwError $ Msg "Expecting at least one decimal digit after the decimal point.")
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
                      (throwError $ Msg "Expecting a positive sign, negative sign, or a decimal digit after the exponent indicator.")
                      (recurse <<< NRExp num true)
              NRExpSign num isPos →
                digitMaybe
                  (throwError $ Msg "Expecting at least one decimal digit.")
                  (recurse <<< NRExp num isPos)
              NRExp num isPos exp →
                digitMaybe
                  (numberEnd (num * pow 10.0 (applySign isPos exp)) parentState)
                  (recurse <<< NRExp num isPos <<< add (exp * 10.0))
              ) numberRead
    )

parseMoreJsonDataT ∷ ∀ m s. Monad m ⇒ S.Source s Char (MaybeT m) ⇒ ParseState → s → m (Tuple (Either ParseException Event) (Tuple ParseState s))
parseMoreJsonDataT parseState = parseJsonNextValueT <<< Tuple parseState

parseJsonT ∷ ∀ m s. Monad m ⇒ S.Source s Char (MaybeT m) ⇒ s → m (Tuple (Either ParseException Event) (Tuple ParseState s))
parseJsonT = parseMoreJsonDataT initParseState

endParseT ∷ ∀ m a. Monad m ⇒ Tuple ParseState a → m (Tuple (Either ParseException Event) (Tuple ParseState a))
endParseT = runStateT $ runExceptT do
  parseState ← getParseState
  case parseState of
    PRoot → throwError $ Msg "Missing a value (null, Boolean, string, number, array, or object)."
    PArrayStart _ → throwError $ Msg "Incomplete array. No close bracket found."
    PObjectStart _ → throwError $ Msg "Incomplete object. No close curly bracket found."
    PArray _ → throwError $ Msg "Missing a value (null, Boolean, string, number, array, or object)."
    PObject _ → throwError $ Msg "Missing a property name in the form of a string."
    PString _ charRead _ →
      case charRead of
        CRClean → throwError $ Msg "Incomplete string. Missing closing double quote"
        CREscape → throwError $ Msg "Incomplete escape. Expected \", \\, /, b, f, n, r, t, or u"
        CRUnicode charCount _ → throwError <<< Msg $ "Incomplete unicode escape. Expected 4 hexidecimal digits. Found only " <> show charCount <> "."
    PLiteral lit charCount _ → throwError <<< Msg $ "Invalid value: "
      <> slice 0 charCount case lit of
        LTrue → "true"
        LFalse → "false"
        LNull → "null"
    PPostName _ → throwError $ Msg "Incomplete property. Missing value."
    PPostNameTermin _ → throwError $ Msg "Incomplete property. Missing value."
    PPostValue parentState →
      case parentState of
        PRoot → throwError Done
        PArray _ → throwError $ Msg "Incomplete array. No close bracket found."
        PObject _ → throwError $ Msg "Incomplete object. No close curly bracket found."
        _ → throwError FlogTheDeveloper
    PNumber numberRead parentState →
      case numberRead of
        NRNegSign → throwError $ Msg "Incomplete number."
        NRWholeNum isPos num → numberEnd (applySign isPos num) parentState
        NRDecPoint _ _ → throwError $ Msg "Incomplete number."
        NRFrac isPos num accF → numberEnd (applySign isPos $ num + accF 0.0) parentState
        NRExpInd _ → throwError $ Msg "Incomplete exponent."
        NRExpSign _ _ → throwError $ Msg "Incomplete exponent."
        NRExp num isPos exp → numberEnd (num * pow 10.0 (applySign isPos exp)) parentState

parseJsonStringT :: forall m. Monad m => String -> m (Tuple (Either ParseException Event) (Tuple ParseState (S.SourcePosition (S.InPlaceString String) S.LineColumnPosition)))
parseJsonStringT str = parseJsonT <<< S.SourcePosition (S.InPlaceString str 0) $ S.LineColumnPosition 0 false 0 0

parseMoreJsonStringT :: forall m. Monad m => Tuple ParseState (S.SourcePosition (S.InPlaceString String) S.LineColumnPosition) -> String -> m (Tuple (Either ParseException Event) (Tuple ParseState (S.SourcePosition (S.InPlaceString String) S.LineColumnPosition)))
parseMoreJsonStringT (Tuple parseState (S.SourcePosition _ lineColPos)) str = parseJsonNextValueT <<< Tuple parseState $ S.SourcePosition (S.InPlaceString str 0) lineColPos
