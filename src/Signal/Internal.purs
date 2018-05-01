module Signal.Internal where

import Prelude hiding (map)
import Data.Array as Array
import Data.Traversable (traverse_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)



newtype Signal eff a = Signal
  { subscribers :: Ref (Array (a -> Eff eff Unit))
  , value :: Ref a
  }

-- | Add a subscribers to the set
subscribe :: forall eff a
           . (a -> Eff (ref :: REF | eff) Unit)
          -> Signal (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) Unit
subscribe f (Signal {subscribers,value}) = do
  x <- readRef value
  modifyRef subscribers (\xs -> Array.snoc xs f)
  f x

-- | Publish a message to the set of subscribers
set :: forall eff a. a -> Signal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x (Signal {subscribers,value}) = do
  writeRef value x
  fs <- readRef subscribers
  traverse_ (\f -> f x) fs

-- | Gets the last message published to the subscribers
get :: forall eff a. Signal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (Signal {value}) = readRef value

-- | Removes all subscribers
clear :: forall eff a
       . Signal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clear (Signal {subscribers}) =
  writeRef subscribers []

-- | Create a signal with a starting value
make :: forall eff a. a -> Eff (ref :: REF | eff) (Signal (ref :: REF | eff) a)
make x = do
  subscribers <- newRef []
  value <- newRef x
  pure (Signal {subscribers,value})
