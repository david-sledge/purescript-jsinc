module Control.Fix where

foreign import fix :: forall a. (a -> a) -> a
