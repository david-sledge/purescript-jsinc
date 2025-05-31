module Data.Target
  ( class Target
  )
  where

import Prelude

class Target t c m where
  pushTarget ∷ c t → m t
