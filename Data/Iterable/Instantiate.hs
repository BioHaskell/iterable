{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
-- | Helpers for instantiating transitive and reflexive instances of Iterable.
module Data.Iterable.Instantiate(self_iterable ,
                                 trans_iterable) where

import           Language.Haskell.TH.Syntax
import           Data.Iterable
import           Data.Proxy(Proxy)

-- | Generates convenience function for iterating over a single object.
--self_iterable typA = gen_iterable typA typA [e| id |] [e| L.singleton |]
self_iterable typA = 
  [d| instance Iterable $(typA) $(typA) where
        itmapM f a     = f a 
        itfoldM  f e a = f e a
        itfoldr  f e a = f a e
        itfoldl  f e a = f e a 
        itfoldl' f e a = f e a
        itlength   d a = 1
    |]

-- | Generates a transitive instance of `Iterable` between $typA and $typC,
--   assuming existence of `Iterable` $typA $typB, and `Iterable` $typB $typC.
trans_iterable typA typB typC = 
  [d| instance Iterable $(typA) $(typC) where
        itmapM   f a   = (itmapM   :: (Monad m) => ( $(typB) -> m $(typB) ) -> $(typA)   -> m $(typA) ) (itmapM f) a 
        itmap    f a   = (itmap    ::              ( $(typB) ->   $(typB) ) -> $(typA)   ->   $(typA) ) (itmap  f) a 
        itfoldM  f e a = (itfoldM  :: (Monad m) => (c -> $(typB)   -> m c) -> c   -> $(typA)   -> m c       ) (itfoldM  f) e a 
        itfoldr  f e a = (itfoldr  ::              ($(typB) -> c   ->   c) -> c   -> $(typA)   ->   c       ) (\bb cc -> itfoldr  f cc bb) e a
        itfoldl  f e a = (itfoldl  ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (itfoldl  f) e a
        itfoldl' f e a = (itfoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (itfoldl' f) e a
        itlength _   a = (itfoldl' ::              (c -> $(typB)   ->   c) -> c   -> $(typA)   ->   c       ) (\a b-> a + itlength (undefined :: Proxy $(typC)) b) 0 a
    |]

