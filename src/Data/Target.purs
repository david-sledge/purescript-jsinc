module Data.Target
  ( class Target
  , pushTarget
  )
  where

class Target t c m where
  pushTarget ∷ c t → m t
