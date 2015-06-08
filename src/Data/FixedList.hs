{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.FixedList where

import GHC.TypeLits

data LengthList :: * -> Nat -> * where
  Head :: LengthList a n
  (:+) :: LengthList a n -> a -> LengthList a (n - 1)

type FixedList a = LengthList a 0

toList :: FixedList a -> [a]
toList = reverse . toList'
  where
    toList' :: LengthList a n -> [a]
    toList' Head      = []
    toList' (xs :+ x) = (:) x $ toList' xs

instance (Show a) => Show (FixedList a) where
  show = show . toList

l1 = Head :: LengthList a 1
l2 = Head :: LengthList a 2
l3 = Head :: LengthList a 3
l4 = Head :: LengthList a 4
l5 = Head :: LengthList a 5
l6 = Head :: LengthList a 6
l7 = Head :: LengthList a 7
l8 = Head :: LengthList a 8
l9 = Head :: LengthList a 9
