{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
--exec s x = snd $ runState s x
exec s = snd . runState s
  -- error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval s = fst . runState s
  -- error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State f
  where f x = (x,x)
  -- error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put y = State f
  where f _ =(() , y)
  -- error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
first' :: (a -> b) -> (a,c) -> (b,c)
first' g (a,c) = (g a, c)

instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f s = State h
    where h x = first' f $ runState s x
    --error "todo: Course.State#(<$>)"

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure x = State s
    where s y = (x, y)
    -- error "todo: Course.State pure#instance (State s)"
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b 
  (<*>) sab sa =  State f
    where f x = ((eval sab x) (eval sa x), (exec sa . exec sab)  x)
    --where f x = ((fst (t1 x)) (fst (t2 x)), (snd . t1 . snd . t2) x)
          -- t1 = (runState sab)
          -- t2 = (runState sa)
    -- error "todo: Course.State (<*>)#instance (State s)"

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f sa = State g
    where g s = (runState . f . fst . runState sa) s (exec sa s)
    -- error "todo: Course.State (=<<)#instance (State s)"

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
-- equalM ::
--   Monad f =>
--   (a -> f Bool) -> a -> f (Optional a)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM f = h
  where h Nil = pure Empty
        h (x :. xs) = g =<< f x
          where g y = if y then pure (Full x) else h xs
-- findM f = h
--   where h Nil = pure Empty
--         h (x :. xs) = (g <$> (f =<< pure x)) <|> (h xs)
--           where g y = if y then Full x else Empty
  -- error "todo: Course.State#findM"

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
ff' :: Ord a => a -> State (S.Set a) Bool
ff' x = State f
  --where f s = ((S.member x s), s)
  where f s = ((S.member x s), (S.delete x s))

-- firstRepeat ::
--   Ord a =>
--   List a
--   -> Optional a
-- firstRepeat Nil = Empty
-- firstRepeat (x :. xs) = eval (findM ff' xs) (S.singleton x) <|> firstRepeat xs

-- (<|>) :: Optional a -> Optional a -> Optional a
-- (<|>) Empty x = x
-- (<|>) x _ = x

ff :: Ord a => a -> State (S.Set a) Bool
ff x = State f
  where f s = (S.member x s, S.insert x s)
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat l = eval (findM ff l) S.empty
  -- error "todo: Course.State#firstRepeat"

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
ff1 :: Ord a => a -> State (S.Set a) Bool
ff1 x = State f
  where f s = ((S.notMember x s), (S.insert x s))

distinct ::
  Ord a =>
  List a
  -> List a
distinct l = (listh . S.toList ) $  exec  (filtering ff1  l) S.empty
  -- error "todo: Course.State#distinct"

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
square :: Int -> Int
square = join (*)

isHappy ::
  Integer
  -> Bool
isHappy i = (contains 1 . firstRepeat) $
            produce (sum . map square  . map digitToInt . show')
            (fromInteger i)
  -- error "todo: Course.State#isHappy"
