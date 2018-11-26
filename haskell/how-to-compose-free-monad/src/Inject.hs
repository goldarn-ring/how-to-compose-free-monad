{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Inject (Inject, inj, prj) where

import Data.Functor.Sum (Sum(..))

class Inject f g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

instance {-# OVERLAPS #-} Inject f (Sum f g) where
  inj = InL
  prj = coproduct Just (const Nothing)

instance {-# OVERLAPPABLE #-} Inject f g => Inject f (Sum h g) where
  inj = InR . inj
  prj = coproduct (const Nothing) prj

instance Inject f f where
  inj = id
  prj = Just

coproduct :: (f a -> b) -> (g a -> b) -> Sum f g a -> b
coproduct f _ (InL a) = f a
coproduct _ g (InR b) = g b
