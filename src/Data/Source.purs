module Data.Source
  ( InPlaceSource(..)
  , LineColumnPosition(..)
  , SourcePosition(..)
  , advance
  , class Position
  , class Source
  , headSource
  , initStringPosition
  , peekSource
  , refillSource
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (charAt, length, slice)
import Data.Tuple (Tuple(Tuple))

class Source s d c m | s → d, d → c where
  peekSource ∷ s → m (Maybe c)
  headSource ∷ s → m (Maybe (Tuple c s))
  refillSource ∷ d → s → m s

instance Monad m ⇒ Source String String Char m where
  peekSource str = pure $ charAt 0 str
  headSource str = peekSource str >>= pure <<< map (flip Tuple $ slice 1 (length str) str)
  refillSource str s = pure $ s <> str

data InPlaceSource s = InPlaceSource s Int

derive instance eqInPlaceSource ∷ Eq s ⇒ Eq (InPlaceSource s)
derive instance ordInPlaceSource ∷ Ord s ⇒ Ord (InPlaceSource s)
derive instance genericInPlaceSource ∷ Generic (InPlaceSource s) _

instance showInPlaceSource ∷ Show s ⇒ Show (InPlaceSource s) where
  show = genericShow

instance Monad m ⇒ Source (InPlaceSource String) String Char m where
  peekSource (InPlaceSource str pos) = pure $ charAt pos str
  headSource s@(InPlaceSource str pos) = peekSource s >>= pure <<< map (flip Tuple <<< InPlaceSource str $ pos + 1)
  refillSource str' (InPlaceSource str pos) = pure $ InPlaceSource (slice pos (length str) str <> str') 0

data SourcePosition s p = SourcePosition s p

derive instance eqSourcePosition ∷ (Eq s, Eq p) ⇒ Eq (SourcePosition s p)
derive instance ordSourcePosition ∷ (Ord s, Ord p) ⇒ Ord (SourcePosition s p)
derive instance genericSourcePosition ∷ Generic (SourcePosition s p) _

instance showSourcePosition ∷ (Show s, Show p) ⇒ Show (SourcePosition s p) where
  show = genericShow

class Position p c m where
  advance ∷ c → p → m p

instance (Monad m, Position p c m, Source s d c m) ⇒ Source (SourcePosition s p) d c m where
  peekSource (SourcePosition s _) = peekSource s
  headSource (SourcePosition s p) = headSource s >>= maybe (pure Nothing) (\ (Tuple c s') → Just <<< Tuple c <<< SourcePosition s' <$> advance c p)
  refillSource d (SourcePosition s p) = refillSource d s >>= pure <<< flip SourcePosition p

data LineColumnPosition = LineColumnPosition Int Boolean Int Int

derive instance eqLineColumnPosition ∷ Eq LineColumnPosition
derive instance ordLineColumnPosition ∷ Ord LineColumnPosition
derive instance genericLineColumnPosition ∷ Generic LineColumnPosition _

instance showLineColumnPosition ∷ Show LineColumnPosition where
  show = genericShow

initStringPosition ∷ ∀ s. s → SourcePosition (InPlaceSource s) LineColumnPosition
initStringPosition str = SourcePosition (InPlaceSource str 0) $ LineColumnPosition 0 false 0 0

instance Applicative m ⇒ Position LineColumnPosition Char m where
  advance c (LineColumnPosition ndx followsCR line col) =
    let f = LineColumnPosition (ndx + 1) in
    pure case c of
      '\r' → f true (line + 1) 0
      '\x12' → f false (line + 1) 0
      '\n' → if followsCR
        then f false line col
        else f false (line + 1) 0
      _ → f false line $ col + 1
