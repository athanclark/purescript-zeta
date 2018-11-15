-- | Signals are a dual concept to Queues - they _always_ have a value that may change,
-- | while a queue represents transient events. This module defines a lower level interface
-- | to a signal - so you can clearly see that registering handlers via `subscribe` is effectful,
-- | as is changing the current value with `set`.

module Signal where

import Signal.Types (kind SCOPE, WRITE, READ, class SignalScope, Handler)

import Prelude hiding (map)
import Data.Array.ST (push, withArray) as Array
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, write, read, modify) as Ref
import Control.Monad.ST (ST)
import Control.Monad.ST (run) as ST



newtype Signal (rw :: # SCOPE) a = Signal
  { subscribers :: Ref (Array (Handler a))
  , value :: Ref a
  }

instance signalScopeSignal :: SignalScope Signal where
  readOnly (Signal xs) = Signal xs
  writeOnly (Signal xs) = Signal xs
  allowReading (Signal xs) = Signal xs
  allowWriting (Signal xs) = Signal xs

-- | Add a subscribers to the set
subscribe :: forall rw a
           . Handler a
          -> Signal (read :: READ | rw) a
          -> Effect Unit
subscribe f sig@(Signal {value}) = do
  subscribeLight f sig
  x <- Ref.read value
  f x

-- | Subscribe without invoking an initial call of the handler
subscribeLight :: forall rw a
                . Handler a
               -> Signal (read :: READ | rw) a
               -> Effect Unit
subscribeLight f (Signal {subscribers}) =
  let go xs =
        let go' :: forall r. ST r (Array (Handler a))
            go' = Array.withArray (Array.push f) xs
        in  ST.run go'
  in  void (Ref.modify go subscribers)


-- FIXME copy all the diffing stuff

-- | Publish a message to the set of subscribers
set :: forall rw a. a -> Signal (write :: WRITE | rw) a -> Effect Unit
set x (Signal {subscribers,value}) = do
  Ref.write x value
  fs <- Ref.read subscribers
  traverse_ (\f -> f x) fs

-- | Gets the last message published to the subscribers
get :: forall rw a. Signal (read :: READ | rw) a -> Effect a
get (Signal {value}) = Ref.read value

-- | Removes all subscribers
clear :: forall rw a. Signal (read :: READ | rw) a -> Effect Unit
clear (Signal {subscribers}) =
  Ref.write [] subscribers

-- | Create a signal with a starting value
make :: forall a. a -> Effect (Signal (read :: READ, write :: WRITE ) a)
make x = do
  subscribers <- Ref.new []
  value <- Ref.new x
  pure (Signal {subscribers,value})
