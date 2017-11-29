module Signal where

import Prelude hiding (map)
import Data.Array as Array
import Data.Traversable (traverse_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)


newtype Signal eff a = Signal
  { subscribers :: Ref (Array (a -> Eff eff Unit))
  , value :: Ref a
  }

subscribe :: forall eff a
           . (a -> Eff (ref :: REF | eff) Unit)
          -> Signal (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) Unit
subscribe f (Signal {subscribers,value}) = do
  x <- readRef value
  f x
  modifyRef subscribers (\xs -> Array.snoc xs f)

set :: forall eff a. a -> Signal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x (Signal {subscribers,value}) = do
  fs <- readRef subscribers
  traverse_ (\f -> f x) fs
  writeRef value x

get :: forall eff a. Signal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (Signal {value}) = readRef value

make :: forall eff a. a -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
make x = do
  subscribers <- newRef []
  value <- newRef x
  pure (Signal {subscribers,value})

map :: forall a b eff
     . (a -> b)
    -> Signal (ref :: REF | eff) a
    -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
map f sig = do
  x <- get sig
  out <- make (f x)
  subscribe (\y -> set (f y) out) sig
  pure out

ap :: forall a b eff
    . (a -> Signal (ref :: REF | eff) b)
   -> Signal (ref :: REF | eff) a
   -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) b)
ap f sig = do
  x <- get sig
  let out = f x
  subscribe (\y -> get (f y) >>= \x' -> set x' out) sig
  pure out

merge :: forall eff a. Signal eff a -> Signal eff a -> Signal eff a
merge s1 s2 = do
  out <- make =<< get s1
  subscribe (\x -> set x out) s1
  subscribe (\x -> set x out) s2
  pure out
