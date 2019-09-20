{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Use where

import Define
import Generics.SOP

--instance Ord (BinTree Int) where
--  compare = $$(gcompare_1)

--compareBinTree :: (Ord a, Ord (BinTree a)) => BinTree a -> BinTree a -> Ordering
--compareBinTree = $$(gcompare_1)

--instance Ord (BinTree Int) where
--  compare = $$(gcompare_2 (POP (Nil :* ((Compa [|| compare ||] :* Compa [|| compare ||] :* Compa [|| compare ||] :* Nil) :* Nil))))
--

--compareBinTree :: (Ord a) => BinTree a -> BinTree a -> Ordering
--compareBinTree = $$(gcompare_2 (POP (Nil :* ((Compa [|| compareBinTree ||] :* Compa [|| compare ||] :* Compa [|| compareBinTree ||] :* Nil) :* Nil))))
--

compareBinTree :: (a -> a -> Ordering) -> BinTree a -> BinTree a -> Ordering
compareBinTree c = $$(gcompare_2 (POP (Nil :* ((Compa [|| compareBinTree c ||] :* Compa [|| c ||] :* Compa [|| compareBinTree c ||] :* Nil) :* Nil))))

instance Ord (BinTree Int) where
  compare = compareBinTree compare

