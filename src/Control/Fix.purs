module Control.Fix where

foreign import fix ∷ ∀ a. (a → a) → a
