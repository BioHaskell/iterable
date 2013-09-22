{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
-- | Helpers for instantiating Iterable for types with Vector containers.
module Data.Iterable.Instantiate.Vector(gen_vector_iterable) where

import           Language.Haskell.TH.Syntax
import           Data.Iterable
import qualified Data.Vector as L

-- | Generates a direct instance of iterable between $typA and $typB with
--   given names of getter and setter, so that:
--   $getter :: $typA -> $typB 
--   $setter :: $typB -> $typA -> $typA
gen_vector_iterable typA typB getter setter = 
  [d| instance Iterable $(typA) $(typB) where
        imapM f a =
          do b' <- L.mapM f ( $(getter) a)
             return $ $(setter) a b'
        ifoldM  f e a  = do r <- L.foldM f e ( $(getter) a)
                            return r
        ifoldr  f e a = L.foldr  f e ( $(getter) a)
        ifoldl  f e a = L.foldl  f e ( $(getter) a)
        ifoldl' f e a = L.foldl' f e ( $(getter) a) 
        ilength   d a = L.length ( $(getter) a)
    |]
