{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Iterable(Iterable(..)) where

import Language.Haskell.TH.Syntax
import Control.Monad.Identity(runIdentity,foldM)

class Iterable a b where
  imapM   :: (Monad m) => (b -> m b) -> a -> m a
  imap    ::              (b ->   b) -> a ->   a
  imap f e = runIdentity $ imapM (return . f) e
  ifoldM  :: (Monad m) => (c -> b -> m c) -> c -> a -> m c
  ifoldr  ::              (b -> c ->   c) -> c -> a ->   c
  ifoldl  ::              (c -> b ->   c) -> c -> a ->   c
  ifoldl' ::              (c -> b ->   c) -> c -> a ->   c
  ilength :: b -> a -> Int -- NOTE: b is 'dummy' type argument to satisfy Iterable a b constraint
