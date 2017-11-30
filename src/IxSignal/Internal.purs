module IxSignal.Internal where

import Prelude hiding (map)
import Data.Traversable (traverse_)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.UUID as UUID
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)



newtype IxSignal eff a = IxSignal
  { subscribers :: Ref (StrMap (a -> Eff eff Unit))
  , value :: Ref a
  }

subscribe :: forall eff a
           . (a -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit)
          -> IxSignal (ref :: REF, uuid :: UUID.GENUUID | eff) a
          -> Eff (ref :: REF, uuid :: UUID.GENUUID | eff) Unit
subscribe f sig = do
  k <- show <$> UUID.genUUID
  subscribeIx f k sig

-- | Add a subscribers to the set
subscribeIx :: forall eff a
           . (a -> Eff (ref :: REF | eff) Unit)
          -> String
          -> IxSignal (ref :: REF | eff) a
          -> Eff (ref :: REF | eff) Unit
subscribeIx f k (IxSignal {subscribers,value}) = do
  x <- readRef value
  f x
  modifyRef subscribers (\xs -> StrMap.insert k f xs)

-- | Publish a message to the set of subscribers
set :: forall eff a. a -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
set x (IxSignal {subscribers,value}) = do
  fs <- readRef subscribers
  traverse_ (\f -> f x) fs
  writeRef value x

setIx :: forall eff a. a -> String -> IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) Unit
setIx x k (IxSignal {subscribers,value}) = do
  mF <- StrMap.lookup k <$> readRef subscribers
  traverse_ (\f -> f x) mF
  writeRef value x

-- | Gets the last message published to the subscribers
get :: forall eff a. IxSignal (ref :: REF | eff) a -> Eff (ref :: REF | eff) a
get (IxSignal {value}) = readRef value

-- | Removes all subscribers
clear :: forall eff a
       . IxSignal (ref :: REF | eff) a
      -> Eff (ref :: REF | eff) Unit
clear (IxSignal {subscribers}) =
  writeRef subscribers StrMap.empty

delete :: forall eff a
        . String -> IxSignal (ref :: REF | eff) a
       -> Eff (ref :: REF | eff) Unit
delete k (IxSignal {subscribers}) =
  modifyRef subscribers (StrMap.delete k)

-- | Create a signal with a starting value
make :: forall eff a. a -> Eff (ref :: REF | eff) (IxSignal (ref :: REF | eff) a)
make x = do
  subscribers <- newRef StrMap.empty
  value <- newRef x
  pure (IxSignal {subscribers,value})
