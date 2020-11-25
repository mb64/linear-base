{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines vectors of known length which can hold linear values.
module Data.V.Linear
  (
  -- * The Data Length Indexed List
    NList(..)
  -- * Simple functions
  -- * Transformations
  -- * Reductions
  ) where

import Prelude.Linear.Internal
import Prelude (Integer)
import Data.Kind (Type, Constraint)
import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- # The Length Indexed List
-------------------------------------------------------------------------------

data NList :: Nat -> Type -> Type where
  Nil :: NList 0 a
  (:|) :: a %1-> NList n a %1-> NList (n+1) a
infixr 5 :|

instance Show a => Show (NList n a) where
  show :: NList n a -> String
  show nl = "[" ++ showNL nl ++ "]" where

    showNL :: Show b => NList m b -> String
    showNL Nil = ""
    showNL (a :| Nil) = show a
    showNL (a :| as) = show a ++ ", " ++ showNL as


-- # Type Level Hackery & Utilitites
-------------------------------------------------------------------------------

data Ur a where
  Ur :: a -> Ur a

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

proxy :: NList n a %1-> (NList n a, Ur (Proxy n))
proxy n = (n, Ur Proxy)

knowPred :: Dict (KnownNat (n+1)) -> Dict (KnownNat n)
knowPred Dict = unsafeCoerce (Dict :: Dict ())


-- # Simple Functions
-------------------------------------------------------------------------------

length :: KnownNat n => NList n a %1-> (NList n a, Integer)
length xs = proxy xs & \(xs', Ur p) -> (xs', natVal p)

{-
-- Alright, this breaks linearity for some unknown reason
snoc :: forall n a. KnownNat n => a %1-> NList n a %1-> NList (n+1) a
snoc a Nil = Cons a Nil
snoc a (x :| xs) = case knowPred (Dict :: Dict (KnownNat (n+1))) of
   Dict ->  x :| (snoc a xs)
-}


-- (++)
-- uncons
-- elem
-- iterate
-- iterate' (strict)
-- replicate (with Proxy)
-- unfoldr
-- splitAt
-- splitWhen
-- partition


-- # Transformations
-------------------------------------------------------------------------------

-- map
-- reverse
-- filter (consumable)
-- zipWith
-- zipWith2
-- zipWith3
-- zip
-- zip2
-- zip3
-- unzip
-- unzip2
-- unzip3


-- # Reductions
-------------------------------------------------------------------------------

-- foldl
-- foldl' (strict version)
-- foldr
-- foldMap (monoidal collapse)
-- foldMap' (strict in accumulation)
-- concat
-- concatMap


-- # Typeclass Instances
-------------------------------------------------------------------------------

-- Typeclasses:
-- Prelude.Functor
-- Prelude.Applicative
-- Data.Functor
-- Data.Applicative
-- Data.Traversible
-- Consumable
-- Dupable
-- Movable

