-- | Module for creating one signal out of another

module IxSignal where

import Signal.Types (READ, WRITE)
import Signal.Internal (make, set, Signal) as Sig
import IxSignal.Internal (IxSignal, subscribeIx, get)

import Prelude ((=<<), bind, pure, (<$>), show, discard)
import Data.UUID (genUUID)
import Effect (Effect)


ixSignalToSignalIx :: forall rw a
                    . IxSignal (read :: READ | rw) a
                   -> String
                   -> Effect (Sig.Signal (read :: READ, write :: WRITE) a)
ixSignalToSignalIx sig k = do
  out <- Sig.make =<< get sig
  subscribeIx (\a -> Sig.set a out) k sig
  pure out


-- | Generates a UUID
ixSignalToSignal :: forall rw a
                  . IxSignal (read :: READ | rw) a
                 -> Effect (Sig.Signal (read :: READ, write :: WRITE) a)
ixSignalToSignal sig = do
  k <- show <$> genUUID
  ixSignalToSignalIx sig k
