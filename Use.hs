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

--instance Ord (BinTree Int) where
--  compare = $$(gcompare_1)

compareBinTree :: (Ord a, Ord (BinTree a)) => BinTree a -> BinTree a -> Ordering
compareBinTree = $$(gcompare_1)

