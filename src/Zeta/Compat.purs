-- | This module tries to mimic purescript-signal's API, without hiding the fact that new Signals
-- | are _created_ in the process of mapping over their value - this means that if you have dangling
-- | subscribers to the old reference, then it won't get garbage collected.

module Zeta.Compat where

import Zeta.Types (READ, WRITE)
import Zeta (Signal, subscribe, make, set, get)

import Prelude (Unit, pure, bind, discard, (=<<), (>>=), ($), class Eq, (<$>), (<*>), when, (/=))
import Data.Array (cons) as Array
import Data.Traversable (traverse_)
import Data.Maybe (Maybe (Just), isJust, fromMaybe)
import Data.Foldable (class Foldable, foldr)
import Effect (Effect)
import Effect.Ref (new, write, read) as Ref


-- | Alias for `make`
constant :: forall a. a -> Effect (Signal (read :: READ, write :: WRITE) a)
constant = make

-- | Creates a new signal, relaying the next incoming values from the old to the new, after transformation.
map' :: forall a b rw
      . (a -> b)
     -> Signal (read :: READ | rw) a
     -> Effect (Signal (read :: READ, write :: WRITE) b)
map' f sig = do
  x <- get sig
  out <- make (f x)
  subscribe (\y -> set (f y) out) sig
  pure out

-- | Same as map, but where each transformation might be effectful.
traverse' :: forall a b rw
           . (a -> Effect b)
          -> Signal (read :: READ | rw) a
          -> Effect (Signal (read :: READ, write :: WRITE) b)
traverse' f sig = do
  out <- make =<< f =<< get sig
  subscribe (\y -> f y >>= \x -> set x out) sig
  pure out

ap' :: forall a b rw rw1
     . Signal (read :: READ | rw) (a -> b)
    -> Signal (read :: READ | rw1) a
    -> Effect (Signal (read :: READ, write :: WRITE) b)
ap' sigF sigX = do
  out <- make =<< (($) <$> get sigF <*> get sigX)
  subscribe (\f -> do
               x <- get sigX
               set (f x) out
            ) sigF
  subscribe (\x -> do
               f <- get sigF
               set (f x) out
            ) sigX
  pure out


bind' :: forall a b rw
       . (a -> Signal (read :: READ, write :: WRITE) b)
      -> Signal (read :: READ | rw) a
      -> Effect (Signal (read :: READ, write :: WRITE) b)
bind' f sig = do
  x <- get sig
  let out = f x
  subscribe (\y -> get (f y) >>= \x' -> set x' out) sig
  pure out

merge :: forall a rw rw1
       . Signal (read :: READ | rw) a
      -> Signal (read :: READ | rw1) a
      -> Effect (Signal (read :: READ, write :: WRITE) a)
merge s1 s2 = do
  out <- make =<< get s1
  subscribe (\x -> set x out) s1
  subscribe (\x -> set x out) s2
  pure out


foldp :: forall a b rw
       . (a -> b -> b)
      -> b
      -> Signal (read :: READ | rw) a
      -> Effect (Signal (read :: READ, write :: WRITE) b)
foldp f i sig = do
  acc <- Ref.new i
  out <- make i
  subscribe (\a -> do
                b <- Ref.read acc
                let b' = f a b
                Ref.write b' acc
                set b' out
            ) sig
  pure out


sampleOn :: forall a b rw rw1
          . Signal (read :: READ | rw) a
         -> Signal (read :: READ | rw1) b
         -> Effect (Signal (read :: READ, write :: WRITE) b)
sampleOn s1 s2 = do
  out <- make =<< get s2
  subscribe (\_ -> get s2 >>= \b -> set b out) s1
  pure out


dropRepeats :: forall a rw
             . Eq a
            => Signal (read :: READ | rw) a
            -> Effect (Signal (read :: READ, write :: WRITE) a)
dropRepeats sig = do
  valRef <- Ref.new =<< get sig
  out <- make =<< get sig
  subscribe (\a -> do
                val <- Ref.read valRef
                when (a /= val) $ do
                  Ref.write a valRef
                  set a out
            ) sig
  pure out


runSignal :: forall rw
           . Signal (read :: READ | rw) (Effect Unit)
          -> Effect Unit
runSignal sig =
  subscribe (\eff -> eff) sig


unwrap :: forall a rw
        . Signal (read :: READ | rw) (Effect a)
       -> Effect (Signal (read :: READ, write :: WRITE) a)
unwrap sig = do
  out <- get sig >>= \eff -> eff >>= make
  subscribe (\eff -> eff >>= \a -> set a out) sig
  pure out


filter :: forall a rw
        . (a -> Boolean)
       -> a
       -> Signal (read :: READ | rw) a
       -> Effect (Signal (read :: READ, write :: WRITE) a)
filter f i sig = do
  out <- do
    x <- get sig
    make =<< if f x then pure x else pure i
  subscribe (\a -> when (f a) $ set a out) sig
  pure out


filterMap :: forall a b rw
           . (a -> Maybe b)
          -> b
          -> Signal (read :: READ | rw) a
          -> Effect (Signal (read :: READ, write :: WRITE) b)
filterMap f i sig =
  map' (fromMaybe i) =<< filter isJust (Just i) =<< map' f sig


flattenArray :: forall a rw
              . Signal (read :: READ | rw) (Array a)
             -> a
             -> Effect (Signal (read :: READ, write :: WRITE) a)
flattenArray sig i = do
  out <- make i
  let feed xs = traverse_ (\x -> set x out) xs
  subscribe feed sig
  pure out


flatten :: forall f a rw
         . Foldable f
        => Signal (read :: READ | rw) (f a)
        -> a
        -> Effect (Signal (read :: READ, write :: WRITE) a)
flatten sig i =
  map' (foldr Array.cons []) sig >>= \out -> flattenArray out i
