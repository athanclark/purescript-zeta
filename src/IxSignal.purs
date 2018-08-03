module IxSignal where

import Signal.Types (READ, WRITE)
import Signal.Internal as Sig
import IxSignal.Internal (IxSignal, subscribeIx, get)

import Prelude
import Data.UUID (GENUUID, genUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)


ixSignalToSignalIx :: forall eff rw a
                    . IxSignal (read :: READ | rw) (ref :: REF | eff) a
                   -> String
                   -> Eff (ref :: REF | eff) 
                      (Sig.Signal (read :: READ, write :: WRITE) (ref :: REF | eff) a)
ixSignalToSignalIx sig k = do
  out <- Sig.make =<< get sig
  subscribeIx (\a -> Sig.set a out) k sig
  pure out


ixSignalToSignal :: forall eff rw a
                  . IxSignal (read :: READ | rw) (ref :: REF, uuid :: GENUUID | eff) a
                 -> Eff (ref :: REF, uuid :: GENUUID | eff)
                    (Sig.Signal (read :: READ, write :: WRITE) (ref :: REF, uuid :: GENUUID | eff) a)
ixSignalToSignal sig = do
  k <- show <$> genUUID
  ixSignalToSignalIx sig k
