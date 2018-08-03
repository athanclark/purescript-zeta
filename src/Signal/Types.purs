module Signal.Types where

import Prelude (Unit)
import Control.Monad.Eff (Eff, kind Effect)


foreign import kind SCOPE

foreign import data WRITE :: SCOPE

foreign import data READ :: SCOPE

type Handler eff a = a -> Eff eff Unit


class SignalScope (s :: # SCOPE -> # Effect -> Type -> Type) where
  readOnly     :: forall rw eff a. s (read  :: READ  | rw) eff a -> s (read  :: READ)  eff a
  writeOnly    :: forall rw eff a. s (write :: WRITE | rw) eff a -> s (write :: WRITE) eff a
  allowReading :: forall rw eff a. s (read  :: READ)  eff a -> s (read  :: READ  | rw) eff a
  allowWriting :: forall rw eff a. s (write :: WRITE) eff a -> s (write :: WRITE | rw) eff a
