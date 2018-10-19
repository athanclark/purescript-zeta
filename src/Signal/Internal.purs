module Signal.Internal where

import Signal.Types (kind SCOPE, WRITE, READ, class SignalScope, Handler)

import Prelude hiding (map)
import Data.Array as Array
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref



newtype Signal (rw :: # SCOPE) a = Signal
  { subscribers :: Ref (Array (a -> Effect Unit))
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
  void (Ref.modify (\xs -> Array.snoc xs f) subscribers)


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
