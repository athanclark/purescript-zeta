module Zeta.Types where

import Prelude (Unit)
import Effect (Effect)


foreign import kind SCOPE

foreign import data WRITE :: SCOPE

foreign import data READ :: SCOPE

type Handler a = a -> Effect Unit

class SignalScope (s :: # SCOPE -> Type -> Type) where
  readOnly     :: forall rw a. s (read  :: READ  | rw) a -> s (read  :: READ)  a
  writeOnly    :: forall rw a. s (write :: WRITE | rw) a -> s (write :: WRITE) a
  allowReading :: forall rw a. s (write :: WRITE | rw) a -> s (read  :: READ, write :: WRITE) a
  allowWriting :: forall rw a. s (read  :: READ  | rw)  a -> s (write :: WRITE, read :: READ)  a
