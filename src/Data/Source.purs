module Data.Source
  ( InPlaceString(..)
  , LineColumnPosition(..)
  , SourcePosition(..)
  , advance
  , class Position
  , class Source
  , headSource
  , peekSource
  )
  where

import Prelude

import Control.Monad.Nope (class MonadNope, liftMaybe)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (charAt, length, slice)
import Data.Tuple (Tuple(Tuple))

class MonadNope m <= Source s c m where
  peekSource ∷ s → m c
  headSource ∷ s → m (Tuple c s)

instance MonadNope m ⇒ Source String Char m where
  peekSource str = liftMaybe $ charAt 0 str
  headSource str = flip Tuple (slice 1 (length str) str) <$> peekSource str

data InPlaceString s = InPlaceString s Int

derive instance eqInPlaceString ∷ Eq s ⇒ Eq (InPlaceString s)
derive instance ordInPlaceString ∷ Ord s ⇒ Ord (InPlaceString s)
derive instance genericInPlaceString ∷ Generic (InPlaceString s) _

instance showInPlaceString ∷ Show s ⇒ Show (InPlaceString s) where
  show = genericShow

instance MonadNope m ⇒ Source (InPlaceString String) Char m where
  peekSource (InPlaceString str pos) = liftMaybe $ charAt pos str
  headSource s@(InPlaceString str pos) = flip Tuple (InPlaceString str $ pos + 1) <$> peekSource s

data SourcePosition s p = SourcePosition s p

derive instance eqSourcePosition ∷ (Eq s, Eq p) ⇒ Eq (SourcePosition s p)
derive instance ordSourcePosition ∷ (Ord s, Ord p) ⇒ Ord (SourcePosition s p)
derive instance genericSourcePosition ∷ Generic (SourcePosition s p) _

instance showSourcePosition ∷ (Show s, Show p) ⇒ Show (SourcePosition s p) where
  show = genericShow

class Position p c m where
  advance ∷ c → p → m p

instance (MonadNope m, Position p c m, Source s c m) ⇒ Source (SourcePosition s p) c m where
  peekSource (SourcePosition s _) = peekSource s
  headSource (SourcePosition s p) = do
    Tuple c s' ← headSource s
    Tuple c <<< SourcePosition s' <$> advance c p

data LineColumnPosition = LineColumnPosition Int Boolean Int Int

derive instance eqLineColumnPosition ∷ Eq LineColumnPosition
derive instance ordLineColumnPosition ∷ Ord LineColumnPosition
derive instance genericLineColumnPosition ∷ Generic LineColumnPosition _

instance showLineColumnPosition ∷ Show LineColumnPosition where
  show = genericShow

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
