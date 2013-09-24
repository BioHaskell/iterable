{-# LANGUAGE MultiParamTypeClasses #-}
-- | Declares Iterable class for handling multi-level, heterogeneous, monomorphic collections that allow nested iteration.
module Data.Iterable(Iterable(..)) where

import Control.Monad.Identity(runIdentity,foldM)

-- | Class for iterating all nested components `b` of type `a`.
class Iterable a b where
  itmapM   :: (Monad m) => (b -> m b) -> a -> m a
  itmap    ::              (b ->   b) -> a ->   a
  itmap f e = runIdentity $ itmapM (return . f) e
  itfoldM  :: (Monad m) => (c -> b -> m c) -> c -> a -> m c
  itfoldr  ::              (b -> c ->   c) -> c -> a ->   c
  itfoldl  ::              (c -> b ->   c) -> c -> a ->   c
  itfoldl' ::              (c -> b ->   c) -> c -> a ->   c
  itlength :: b -> a -> Int -- NOTE: b is 'dummy' type argument to satisfy Iterable a b constraint
