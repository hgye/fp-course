{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
firstF :: Functor f =>
  (a -> b) -> f (a, c) -> f (b, c)
firstF f = (<$>) (first' f)

instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) f s = StateT h
    where h x = (<$>) (first' f) (runStateT s x)
    -- error "todo: Course.StateT (<$>)#instance (StateT s f)"

-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure s = StateT g
    where g x = pure (s, x)
    -- error "todo: Course.StateT pure#instance (StateT s f)"
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) sab sa = StateT g
    where g s = go =<< runStateT sab s
          go p = first' (fst p) <$> runStateT sa (snd p)
  --(<*>) sab sa = StateT g
    -- where g s = sb
    --         where sb = h <$> (runStateT sab s) <*> (runStateT sa s)
    --               h x y = first' (fst x) (fst y, snd x)
  --   error "todo: Course.StateT (<*>)#instance (StateT s f)"

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f sa = StateT g
    where g s = go =<< runStateT sa s
          go p = runStateT (f (fst p)) (snd p)
    -- error "todo: Course.StateT (=<<)#instance (StateT s f)"

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT g
  where g s = ExactlyOne (f s)
  -- error "todo: Course.StateT#state'"

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' sa s = runExactlyOne $ runStateT sa s
  -- error "todo: Course.StateT#runState'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT sa s = snd <$> runStateT sa s
  -- error "todo: Course.StateT#execT"

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
exec' sa s = snd $ runState' sa s
  -- error "todo: Course.StateT#exec'"

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT sa s = fst <$> runStateT sa s
  -- error "todo: Course.StateT#evalT"

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
eval' sa s = fst $ runState' sa s
  -- error "todo: Course.StateT#eval'"

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT f
  where f x = pure (x, x)
  -- error "todo: Course.StateT#getT"

-- | A `StateT` where the resulting state is seeded with the given value.
-- 
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT s = StateT g
  where g _ = pure ((), s)
  -- error "todo: Course.StateT#putT"

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' l = (listh . S.toList ) $  exec  (filtering ff1  l) S.empty
  -- error "todo: Course.StateT#distinct'"

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
ff2 :: (Ord a, Num a) => a -> StateT (S.Set a) Optional Bool
ff2 x = StateT f
  where f s = case x > 100 of
          True -> Empty
          False -> Full (S.notMember x s, S.insert x s)

distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF l = f $  execT  (filtering ff2  l) S.empty
  where f Empty = Empty
        f x = (listh . S.toList) <$> x
  -- error "todo: Course.StateT#distinctF"

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) ::
    (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  (<$>) h o = OptionalT g
    where g =  lift1 h <$> runOptionalT o
    -- where g = ((<$>) h) <$> (runOptionalT o)
    -- error "todo: Course.StateT (<$>)#instance (OptionalT f)"

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure x = OptionalT g
    where g = pure . pure $ x
    -- error "todo: Course.StateT pure#instance (OptionalT f)"
  (<*>) ::
    OptionalT f (a -> b)
    -> OptionalT f a
    -> OptionalT f b
  (<*>) oab oa = OptionalT g
    where g = onFull go =<< runOptionalT oab
          go h = (<$>) h <$> runOptionalT oa
  -- (<*>) oab oa = OptionalT g
  --   where g = go =<< runOptionalT oab
  --         go Empty = pure Empty
  --         go h = ((<$>) . (<*>)) h (runOptionalT oa)
  -- -- (<*>) oab oa =  OptionalT g
  --   where g = go =<< ((onFull runOptionalT) . pure) oab
  --         go Empty = pure Empty
  --         go h = ((<$>) . (<*>)) h (((onFull runOptionalT) . pure) oa)
  -- error "todo: Course.StateT (<*>)#instance (OptionalT f)"

-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  (=<<) g oa = OptionalT h
    where h = onFull go =<< runOptionalT oa
          go x = runOptionalT (g x)
    -- error "todo: Course.StateT (=<<)#instance (OptionalT f)"

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) f (Logger l x) = Logger l (f x)
    -- error "todo: Course.StateT (<$>)#instance (Logger l)"

-- | Implement the `Applicative` instance for `Logger`. 
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger (listh [])
    -- error "todo: Course.StateT pure#instance (Logger l)"
  (<*>) (Logger l1 x1) (Logger l2 x2) =
    Logger (l1 ++ l2) (x1 x2)
    -- error "todo: Course.StateT (<*>)#instance (Logger l)"

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) f (Logger la x) = Logger (la ++ getl lab) (getv lab)
    where getl (Logger l _) = l
          getv (Logger _ v) = v
          lab = f x
    -- error "todo: Course.StateT (=<<)#instance (Logger l)"

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l  = Logger (l :. Nil)
  -- error "todo: Course.StateT#log1"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
ff3 :: (Integral a, Show a) =>
  a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
ff3 x = StateT f
  where f s = OptionalT go
          where go = case x of
                       n | n > 100 -> log1 ("aborting > 100: " ++ show' x) Empty
                       n | S.member n s && even n
                           -> log1 ("even number: " ++ show' x) (Full (False, s))
                       n | S.member n s -> pure (Full (False, s))
                       n | S.notMember n s && even n
                           -> log1 ("even number: " ++ show' x) (Full (True, S.insert x s))
                       _ -> pure (Full (True, S.insert x s))

distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG l = runOptionalT $ evalT (filtering ff3 l) S.empty
  -- error "todo: Course.StateT#distinctG"

onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
