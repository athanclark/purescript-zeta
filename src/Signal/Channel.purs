module Signal.Channel where

import Signal.Internal (Signal, set, make)

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)


newtype Channel eff a = Channel (Signal eff a)


channel :: forall eff a. a -> Eff (ref :: REF | eff) (Channel (ref :: REF | eff) a)
channel x = Channel <$> make x


send :: forall eff a. Channel (ref :: REF | eff) a -> a -> Eff (ref :: REF | eff) Unit
send (Channel sig) x = set x sig


subscribe :: forall eff a. Channel eff a -> Signal eff a
subscribe (Channel sig) = sig
