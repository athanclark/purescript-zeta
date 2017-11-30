module Signal where

import Signal.Internal (Signal, subscribe, make, set, get)

import Prelude hiding (map)
import Data.Array as Array
import Data.Traversable (traverse_)
import Data.Maybe (Maybe (Just), isJust, fromMaybe)
import Data.Foldable (class Foldable, foldr)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Execution.Immediate (IMMEDIATE, run0)

-- | Alias for `make`
constant :: forall eff a. a -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
constant = make

-- | Creates a new signal, relaying the next incoming values from the old to the new, after transformation.
map' :: forall a b eff
      . (a -> b)
     -> Signal (ref :: REF | eff) a
     -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
map' f sig = do
  x <- get sig
  out <- make (f x)
  subscribe (\y -> set (f y) out) sig
  pure out

-- | Same as map, but where each transformation might be effectful.
traverse' :: forall a b eff
           . (a -> Eff (ref :: REF | eff) b)
          -> Signal (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
traverse' f sig = do
  out <- make =<< f =<< get sig
  subscribe (\y -> f y >>= \x -> set x out) sig
  pure out

ap' :: forall a b eff
     . Signal (ref :: REF | eff) (a -> b)
    -> Signal (ref :: REF | eff) a
    -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
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


bind' :: forall a b eff
       . (a -> Signal (ref :: REF | eff) b)
      -> Signal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
bind' f sig = do
  x <- get sig
  let out = f x
  subscribe (\y -> get (f y) >>= \x' -> set x' out) sig
  pure out

merge :: forall eff a
       . Signal (ref :: REF | eff) a
      -> Signal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
merge s1 s2 = do
  out <- make =<< get s1
  subscribe (\x -> set x out) s1
  subscribe (\x -> set x out) s2
  pure out


foldp :: forall eff a b
       . (a -> b -> b)
      -> b
      -> Signal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
foldp f i sig = do
  acc <- newRef i
  out <- make i
  subscribe (\a -> do
                b <- readRef acc
                let b' = f a b
                writeRef acc b'
                set b' out
            ) sig
  pure out


sampleOn :: forall eff a b
          . Signal (ref :: REF | eff) a
         -> Signal (ref :: REF | eff) b
         -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
sampleOn s1 s2 = do
  out <- make =<< get s2
  subscribe (\_ -> get s2 >>= \b -> set b out) s1
  pure out


dropRepeats :: forall eff a
             . Eq a
            => Signal (ref :: REF | eff) a
            -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
dropRepeats sig = do
  valRef <- newRef =<< get sig
  out <- make =<< get sig
  subscribe (\a -> do
                val <- readRef valRef
                when (a /= val) $ do
                  writeRef valRef a
                  set a out
            ) sig
  pure out


runSignal :: forall eff
           . Signal (ref :: REF | eff) (Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit
runSignal sig =
  subscribe (\eff -> eff) sig


unwrap :: forall eff a
        . Signal (ref :: REF | eff) (Eff (ref :: REF | eff) a)
       -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
unwrap sig = do
  out <- get sig >>= \eff -> eff >>= make
  subscribe (\eff -> eff >>= \a -> set a out) sig
  pure out


filter :: forall eff a
        . (a -> Boolean)
       -> a
       -> Signal (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
filter f i sig = do
  out <- do
    x <- get sig
    make =<< if f x then pure x else pure i
  subscribe (\a -> when (f a) $ set a out) sig
  pure out


filterMap :: forall eff a b
           . (a -> Maybe b)
          -> b
          -> Signal (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
filterMap f i sig =
  map' (fromMaybe i) =<< filter isJust (Just i) =<< map' f sig


flattenArray :: forall eff a
              . Signal (ref :: REF, immediate :: IMMEDIATE | eff) (Array a)
             -> a
             -> Eff ( ref :: REF
                    , immediate :: IMMEDIATE
                    | eff) (Signal (ref :: REF, immediate :: IMMEDIATE | eff) a)
flattenArray sig i = do
  out <- make i
  let feed xs = traverse_ (\x -> run0 (set x out)) xs
  subscribe feed sig
  pure out


flatten :: forall eff f a
         . Foldable f
         => Signal (ref :: REF, immediate :: IMMEDIATE | eff) (f a)
         -> a
         -> Eff (ref :: REF, immediate :: IMMEDIATE | eff) (Signal (ref :: REF, immediate :: IMMEDIATE | eff) a)
flatten sig i =
  map' (foldr Array.cons []) sig >>= \out -> flattenArray out i
