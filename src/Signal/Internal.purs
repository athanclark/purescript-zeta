module Signal.Internal where

import Signal.Types (kind SCOPE, WRITE, READ, class SignalScope)

import Prelude hiding (map)
import Data.Array as Array
import Data.Traversable (traverse_)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)



newtype Signal (rw :: # SCOPE) (eff :: # Effect) a = Signal
  { subscribers :: Ref (Array (a -> Eff eff Unit))
  , value :: Ref a
  }

instance signalScopeSignal :: SignalScope Signal where
  readOnly (Signal xs) = Signal xs
  writeOnly (Signal xs) = Signal xs
  allowReading (Signal xs) = Signal xs
  allowWriting (Signal xs) = Signal xs

-- | Add a subscribers to the set
subscribe :: forall eff rw a
           . (a -> Eff (ref :: REF | eff) Unit)
          -> Signal (read :: READ | rw) (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) Unit
subscribe f sig@(Signal {value}) = do
  subscribeLight f sig
  x <- readRef value
  f x

subscribeLight :: forall eff rw a
                . (a -> Eff (ref :: REF | eff) Unit)
               -> Signal (read :: READ | rw) (ref :: REF | eff) a
               -> Eff (ref :: REF | eff) Unit
subscribeLight f (Signal {subscribers}) = do
  modifyRef subscribers (\xs -> Array.snoc xs f)


-- | Publish a message to the set of subscribers
set :: forall eff rw a
     . a -> Signal (write :: WRITE | rw) (ref :: REF | eff) a
    -> Eff (ref :: REF | eff) Unit
set x (Signal {subscribers,value}) = do
  writeRef value x
  fs <- readRef subscribers
  traverse_ (\f -> f x) fs

-- | Gets the last message published to the subscribers
get :: forall eff rw a
     . Signal (read :: READ | rw) (ref :: REF | eff) a
    -> Eff (ref :: REF | eff) a
get (Signal {value}) = readRef value

-- | Removes all subscribers
clear :: forall eff rw a
       . Signal (read :: READ | rw) (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clear (Signal {subscribers}) =
  writeRef subscribers []

-- | Create a signal with a starting value
make :: forall eff a
      . a -> Eff (ref :: REF | eff) (Signal (read :: READ, write :: WRITE ) (ref :: REF | eff) a)
make x = do
  subscribers <- newRef []
  value <- newRef x
  pure (Signal {subscribers,value})
