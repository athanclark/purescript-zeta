-- | Signals are a dual concept to Queues - they _always_ have a value that may change,
-- | while a queue represents transient events. This module defines a lower level interface
-- | to a signal - so you can clearly see that registering handlers via `subscribe` is effectful,
-- | as is changing the current value with `set`.

module Zeta where

import Zeta.Types (kind SCOPE, WRITE, READ, class SignalScope, Handler)

import Prelude (Unit, bind, void, class Eq, discard, pure, ($), (=<<), when, (/=))
import Data.Array.ST (push, withArray) as Array
import Data.Traversable (traverse_)
import Data.Maybe (Maybe (..))
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


-- | Fires the handler on the initial value, and successively only when the value changes
-- | with respect to `Eq`.
subscribeDiff :: forall rw a
               . Eq a
               => Handler a
               -> Signal (read :: READ | rw) a
               -> Effect Unit
subscribeDiff f sig = do
  lastValueRef <- Ref.new Nothing
  let go x = do
        lastValue <- Ref.read lastValueRef
        when (Just x /= lastValue) $ do
          Ref.write (Just x) lastValueRef
          f x
  subscribe go sig


-- | Does not fire the handler on the initial value - only waits until it changes with
-- | respect to `Eq`.
subscribeDiffLight :: forall rw a
               . Eq a
               => Handler a
               -> Signal (read :: READ | rw) a
               -> Effect Unit
subscribeDiffLight f sig = do
  lastValueRef <- Ref.new =<< get sig
  let go x = do
        lastValue <- Ref.read lastValueRef
        when (x /= lastValue) $ do
          Ref.write x lastValueRef
          f x
  subscribeLight go sig



-- | Publish a message to the set of subscribers
set :: forall rw a. a -> Signal (write :: WRITE | rw) a -> Effect Unit
set x (Signal {subscribers,value}) = do
  Ref.write x value
  fs <- Ref.read subscribers
  traverse_ (\f -> f x) fs

-- | Only set the value if it differs from the current one - useful if you don't want
-- | each handler individually to attempt diffing
setDiff :: forall a. Eq a => a -> Signal (read :: READ, write :: WRITE) a -> Effect Unit
setDiff x sig = do
  y <- get sig
  when (y /= x) (set x sig)


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
