module Compiler.Lens
  ( preorder
  , postorder
  ) where

preorder :: Monad m => ((t1 -> m b) -> t2 -> m b) -> (t1 -> m t2) -> t1 -> m b
preorder children f = pre
  where pre a = f a >>= children pre

postorder :: Monad m => ((t -> m b) -> t -> m a) -> (a -> m b) -> t -> m b
postorder children f = post
  where post a = children post a >>= f
