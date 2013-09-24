{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
-- | Helpers for instantiating Iterable for types with Vector containers.
module Data.Iterable.Instantiate.Vector(gen_vector_iterable) where

import           Language.Haskell.TH.Syntax
import           Data.Iterable
import qualified Data.Vector as V

-- | Generates a direct instance of iterable between $typA and $typB with
--   given names of getter and setter, so that:
--   $getter :: $typA -> $typB 
--   $setter :: $typB -> $typA -> $typA
gen_vector_iterable typA typB getter setter = 
  [d| instance Iterable $(typA) $(typB) where
        itmapM f a =
          do b' <- V.mapM f ( $(getter) a)
             return $ $(setter) a b'
        itfoldM  f e a  = do r <- V.foldM f e ( $(getter) a)
                             return r
        itfoldr  f e a = V.foldr  f e ( $(getter) a)
        itfoldl  f e a = V.foldl  f e ( $(getter) a)
        itfoldl' f e a = V.foldl' f e ( $(getter) a) 
        itlength   d a = V.length ( $(getter) a)
    |]
