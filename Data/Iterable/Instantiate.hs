{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
-- | Helpers for instantiating transitive and reflexive instances of Iterable.
module Data.Iterable.Instantiate(self_iterable ,
                                 trans_iterable) where

import           Language.Haskell.TH.Syntax
import           Data.Iterable

-- | Generates convenience function for iterating over a single object.
--self_iterable typA = gen_iterable typA typA [e| id |] [e| L.singleton |]
self_iterable typA = 
  [d| instance Iterable $(typA) $(typA) where
        imapM f a     = f a 
        ifoldM  f e a = f e a
        ifoldr  f e a = f a e
        ifoldl  f e a = f e a 
        ifoldl' f e a = f e a
        ilength   d a = 1
    |]

-- | Generates a transitive instance of `Iterable` between $typA and $typC,
--   assuming existence of `Iterable` $typA $typB, and `Iterable` $typB $typC.
trans_iterable typA typB typC = 
  [d| instance Iterable $(typA) $(typC) where
        imapM   f a   = (imapM   :: (Monad m) => ( $(typB) -> m $(typB) ) -> $(typA)   -> m $(typA) ) (imapM f) a 
        imap    f a   = (imap    ::              ( $(typB) ->   $(typB) ) -> $(typA)   ->   $(typA) ) (imap  f) a 
        ifoldM  f e a = (ifoldM  :: (Monad m) => (c -> $(typB)   -> m c) -> c   -> $(typA)   -> m c       ) (ifoldM  f) e a 
        ifoldr  f e a = (ifoldr  ::              ($(typB) -> c   ->   c) -> c   -> $(typA)   ->   c       ) (\bb cc -> ifoldr  f cc bb) e a
        ifoldl  f e a = (ifoldl  ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (ifoldl  f) e a
        ifoldl' f e a = (ifoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (ifoldl' f) e a
        ilength _   a = (ifoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (\a b-> a + ilength (undefined :: $(typC)) b) 0 a
    |]

