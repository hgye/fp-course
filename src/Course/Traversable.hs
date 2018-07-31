{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne x) = ExactlyOne <$> f x
    -- error "todo: Course.Traversable traverse#instance ExactlyOne"

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full x) = Full <$> f x
    -- error "todo: Course.Traversable traverse#instance Optional"

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id 
  --error "todo: Course.Traversable#sequenceA"

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse h (Compose ca) = Compose <$> sequenceA (sequenceA <$> go)
    where go = (<$>) h <$> ca
  --   error "todo: Course.Traversable traverse#instance (Compose f g)"

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) f1 (Product fa ga) = Product (f1 <$> fa) (f1 <$> ga)
    -- error "todo: Course.Traversable (<$>)#instance (Product f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse f1 (Product fa ga) = Product <$> sequenceA (f1 <$> fa) <*> sequenceA (f1 <$> ga)
    -- error "todo: Course.Traversable traverse#instance (Product f g)"

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) f (InL x)= InL (f <$> x)
  (<$>) f (InR x)= InR (f <$> x)
    -- error "todo: Course.Traversable (<$>)#instance (Coproduct f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse f1 (InL fa) = InL <$> sequenceA (f1 <$> fa)
  traverse f1 (InR fa) = InR <$> sequenceA (f1 <$> fa)
    -- error "todo: Course.Traversable traverse#instance (Coproduct f g)"
